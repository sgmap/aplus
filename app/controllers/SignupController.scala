package controllers

import actions.{LoginAction, RequestWithUserData}
import cats.data.EitherT
import cats.syntax.all._
import constants.Constants
import controllers.Operators.UserOperators
import helper.{Time, UUIDHelper}
import helper.PlayFormHelper.formErrorsLog
import helper.ScalatagsHelpers.writeableOf_Modifier
import java.time.{Instant, ZonedDateTime}
import java.util.UUID
import javax.inject.{Inject, Singleton}
import models.{Authorization, Error, EventType, SignupRequest, User}
import models.formModels.{AddSignupsFormData, SignupFormData}
import org.webjars.play.WebJarsUtil
import play.api.Configuration
import play.api.data.Form
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, InjectedController, Request, Result}
import scala.concurrent.{ExecutionContext, Future}
import serializers.Keys
import services.{EventService, NotificationService, SignupService, UserGroupService, UserService}

@Singleton
case class SignupController @Inject() (
    configuration: Configuration,
    eventService: EventService,
    groupService: UserGroupService,
    loginAction: LoginAction,
    notificationsService: NotificationService,
    signupService: SignupService,
    userService: UserService,
)(implicit ec: ExecutionContext, webJarsUtil: WebJarsUtil)
    extends InjectedController
    with I18nSupport
    with UserOperators {

  def signupForm: Action[AnyContent] =
    withSignupInSession { implicit request => signupRequest =>
      eventService.logSystem(
        EventType.SignupFormShowed,
        s"Visualisation de la page d'inscription ${signupRequest.id}"
      )
      groupService.all.map(groups =>
        Ok(views.signup.page(signupRequest, SignupFormData.form, groups))
      )
    }

  def createSignup: Action[AnyContent] =
    withSignupInSession { implicit request => signupRequest =>
      SignupFormData.form
        .bindFromRequest()
        .fold(
          formWithError => {
            eventService.logSystem(
              EventType.SignupFormValidationError,
              s"Erreurs dans le formulaire d'inscription ${signupRequest.id} : ${formErrorsLog(formWithError)}"
            )
            groupService.all
              .map(groups => BadRequest(views.signup.page(signupRequest, formWithError, groups)))
          },
          form => {
            // `SignupFormData` is supposed to validate that boolean,
            // we put a require here "just in case"...
            require(form.cguChecked, "Le formulaire SignupFormData doit avoir cguChecked === true")
            // Note: see also `User.validateWith`
            val name: String =
              if (form.sharedAccount) form.sharedAccountName.orEmpty
              else s"${form.lastName.orEmpty.toUpperCase} ${form.firstName.orEmpty}"
            val user = User(
              id = UUID.randomUUID(),
              key = "", // generated by UserService
              firstName = form.firstName,
              lastName = form.lastName,
              name = name,
              qualite = form.qualite.orEmpty,
              email = signupRequest.email,
              helper = true,
              instructor = false,
              admin = false,
              areas = form.areaId :: Nil,
              creationDate = Time.nowParis(),
              communeCode = "0",
              groupAdmin = false,
              disabled = false,
              expert = false,
              groupIds = form.groupId :: Nil,
              cguAcceptationDate = Time.nowParis().some,
              newsletterAcceptationDate = None,
              phoneNumber = form.phoneNumber,
              observableOrganisationIds = Nil,
              sharedAccount = form.sharedAccount,
              internalSupportComment = None
            )
            userService
              .addFuture(user :: Nil)
              .flatMap(
                _.fold(
                  errorMessage => {
                    // Here, a few errors are possible. Barring transient errors,
                    // this is most likely a case of the email being already used for a user.
                    eventService.logSystem(
                      EventType.SignupFormError,
                      s"Erreur lors de l'ajout d'un utilisateur par préinscription ${signupRequest.id} : " +
                        errorMessage +
                        s" Utilisateur ayant échoué : ${user.toLogString}"
                    )
                    val userMessage =
                      "Une erreur imprévue est survenue. Ce n’est pas de votre faute. " +
                        "Nous vous invitons à réessayer plus tard, " +
                        s"ou contacter le support ${Constants.supportEmail} si celle-ci persiste."
                    val formWithError = SignupFormData.form.fill(form).withGlobalError(userMessage)
                    groupService.all
                      .map(groups =>
                        InternalServerError(views.signup.page(signupRequest, formWithError, groups))
                      )
                  },
                  _ =>
                    LoginAction.readUserRights(user).map { userRights =>
                      eventService.log(
                        EventType.SignupFormSuccessful,
                        s"Utilisateur créé via le formulaire d'inscription ${signupRequest.id} " +
                          s"(créateur de la préinscription : ${signupRequest.invitingUserId}). " +
                          s"Utilisateur : ${user.toLogString}",
                        involvesUser = signupRequest.invitingUserId.some
                      )(new RequestWithUserData(user, userRights, request))
                      Redirect(routes.HomeController.welcome)
                        .withSession(
                          request.session - Keys.Session.signupId + (Keys.Session.userId -> user.id.toString)
                        )
                        .flashing(
                          "success" -> "Votre compte est maintenant créé. Merci d’utiliser Administration+."
                        )
                    }
                )
              )
          }
        )
    }

  def signupRequests: Action[AnyContent] =
    loginAction.async { implicit request: RequestWithUserData[AnyContent] =>
      asUserWithAuthorization(Authorization.canSeeSignupsPage) { () =>
        EventType.SignupsUnauthorized -> s"Accès non autorisé à la page des préinscriptions"
      } { () =>
        showAllSignupsPage(AddSignupsFormData.form)
      }
    }

  def addSignupRequests: Action[AnyContent] =
    loginAction.async { implicit request =>
      asUserWithAuthorization(Authorization.canSeeSignupsPage) { () =>
        EventType.SignupsUnauthorized -> s"Tentative non autorisée d'ajout de préinscriptions"
      } { () =>
        AddSignupsFormData.form
          .bindFromRequest()
          .fold(
            formWithError => {
              eventService.log(
                EventType.SignupsValidationError,
                s"Erreurs dans le formulaire d'ajout de préinscriptions : ${formErrorsLog(formWithError)}"
              )
              showAllSignupsPage(formWithError)
            },
            form => {
              val emails: List[String] =
                form.emails.split('\n').toList.map(_.trim).filter(_.nonEmpty)
              // Note: We also want the disabled Users here
              EitherT(userService.byEmailsFuture(emails))
                .product(EitherT(signupService.byEmails(emails)))
                .flatMap { case (existingUsers, existingSignups) =>
                  val nonExistingEmails = emails.filterNot(email =>
                    existingUsers.exists(_.email.toLowerCase === email.toLowerCase) ||
                      existingSignups.exists(_.email.toLowerCase === email.toLowerCase)
                  )
                  val now = Instant.now()
                  val newSignups: List[SignupRequest] =
                    nonExistingEmails.map(email =>
                      SignupRequest(
                        id = UUID.randomUUID(),
                        requestDate = now,
                        email = email,
                        invitingUserId = request.currentUser.id
                      )
                    )

                  val insertRequests: Future[List[(Either[Error, Unit], SignupRequest)]] =
                    if (form.dryRun)
                      Future.successful(newSignups.map(signup => (().asRight, signup)))
                    else
                      Future
                        .traverse(newSignups)(signup =>
                          signupService.addSignupRequest(signup).map(result => (result, signup))
                        )

                  EitherT(
                    insertRequests
                      .flatMap { results =>
                        val errors: List[(SignupRequest, Error)] = results
                          .flatMap { case (result, signup) =>
                            result.left.toOption.map(error => (signup, error))
                          }
                        errors.foreach { case (_, error) => eventService.logError(error) }
                        val successes: List[SignupRequest] = results
                          .flatMap { case (result, signup) =>
                            result.toOption.map(_ => signup)
                          }
                        successes.foreach { signup =>
                          notificationsService.newSignup(signup)
                          eventService.log(
                            EventType.SignupCreated,
                            s"Préinscription créée ${signup.toLogString}"
                          )
                        }

                        // Important note: we deliberately
                        // **do not show errors as validation errors**
                        // this is intended to be quicker to use as the batch of working emails
                        // is added directly and non working ones
                        // can be copy/pasted somewhere to be processed later
                        showAllSignupsPage(
                          AddSignupsFormData.form,
                          successSignups = successes,
                          existingSignups = existingSignups,
                          existingUsers = existingUsers,
                          miscErrors = errors
                        ).map(_.asRight[Error])
                      }
                  )
                }
                .valueOrF { error =>
                  eventService.logError(error)
                  showAllSignupsPage(AddSignupsFormData.form.fill(form))
                }
            }
          )
      }
    }

  private def showAllSignupsPage(
      form: Form[AddSignupsFormData],
      successSignups: List[SignupRequest] = Nil,
      existingSignups: List[SignupRequest] = Nil,
      existingUsers: List[User] = Nil,
      miscErrors: List[(SignupRequest, Error)] = Nil
  )(implicit request: RequestWithUserData[AnyContent]): Future[Result] =
    signupService
      .allAfter(ZonedDateTime.now().minusYears(1).toInstant)
      .map(
        _.fold(
          { error =>
            eventService.logError(error)
            InternalServerError(
              views.signupAdmin.page(request.currentUser, request.rights, Nil, form)
            )
              .flashing(
                "error" -> (s"Une erreur serveur est survenue ! " +
                  "Nous ne pouvons pas afficher les préinscriptions.")
              )
          },
          signups =>
            Ok(
              views.signupAdmin.page(
                request.currentUser,
                request.rights,
                signups,
                form,
                successSignups = successSignups,
                existingSignups = existingSignups,
                existingUsers = existingUsers,
                miscErrors = miscErrors
              )
            )
        )
      )

  /** Note: parameter is curried to easily mark `Request` as implicit. */
  private def withSignupInSession(
      action: Request[_] => SignupRequest => Future[Result]
  ): Action[AnyContent] =
    Action.async { implicit request =>
      val signupOpt = request.session.get(Keys.Session.signupId).flatMap(UUIDHelper.fromString)
      signupOpt match {
        case None =>
          val message = "Merci de vous connecter pour accéder à cette page."
          Future.successful(
            Redirect(routes.LoginController.login)
              .flashing("error" -> message)
              .withSession(request.session - Keys.Session.signupId)
          )
        case Some(signupId) =>
          signupService
            .byId(signupId)
            .flatMap(
              _.fold(
                e => {
                  eventService.logErrorNoUser(e)
                  Future.successful(
                    Redirect(routes.LoginController.login)
                      .flashing("error" -> Constants.error500FlashMessage)
                      .withSession(request.session - Keys.Session.signupId)
                  )
                },
                {
                  case None =>
                    eventService.logSystem(
                      EventType.MissingSignup,
                      s"Tentative d'inscription avec l'id $signupId en session, mais n'existant pas en BDD"
                    )
                    val message = "Une erreur interne est survenue. " +
                      "Si celle-ci persiste, vous pouvez contacter le support Administration+."
                    Future.successful(
                      Redirect(routes.LoginController.login)
                        .flashing("error" -> message)
                        .withSession(request.session - Keys.Session.signupId)
                    )
                  case Some(signupRequest) =>
                    userService.byEmailFuture(signupRequest.email).flatMap {
                      case None               => action(request)(signupRequest)
                      case Some(existingUser) =>
                        // The user exists already, we exchange its signup session by a user session
                        // (this case happen if the signup session has not been purged after user creation)
                        Future.successful(
                          Redirect(routes.HomeController.welcome)
                            .withSession(
                              request.session - Keys.Session.userId - Keys.Session.signupId +
                                (Keys.Session.userId -> existingUser.id.toString)
                            )
                        )
                    }
                }
              )
            )
      }
    }

}
