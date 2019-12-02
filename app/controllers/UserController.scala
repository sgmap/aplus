package controllers

import java.util.{Locale, UUID}

import actions.{LoginAction, RequestWithUserData}
import extentions.Operators.{GroupOperators, UserOperators, not}
import extentions.{Time, UUIDHelper}
import javax.inject.{Inject, Singleton}
import models.{Area, User, UserGroup}
import org.joda.time.{DateTime, DateTimeZone}
import org.postgresql.util.PSQLException
import org.webjars.play.WebJarsUtil
import play.api.Configuration
import play.api.data.Forms.{optional, text, tuple, _}
import play.api.data.validation.Constraints.{maxLength, nonEmpty}
import play.api.data.{Form, FormError, Mapping}
import play.api.mvc._
import play.filters.csrf.CSRF
import play.filters.csrf.CSRF.Token
import services._

@Singleton
case class UserController @Inject()(loginAction: LoginAction,
                                    userService: UserService,
                                    groupService: UserGroupService,
                                    applicationService: ApplicationService,
                                    notificationsService: NotificationService,
                                    configuration: Configuration,
                                    eventService: EventService)(implicit val webJarsUtil: WebJarsUtil) extends InjectedController with play.api.i18n.I18nSupport with UserOperators with GroupOperators {

  def all(areaId: UUID): Action[AnyContent] = loginAction { implicit request: RequestWithUserData[AnyContent] =>
    asUserWhoSeesUsersOfArea(areaId) { () =>
      "ALL_USER_UNAUTHORIZED" -> "Accès non autorisé à l'admin des utilisateurs"
    } { () =>
      val selectedArea = Area.fromId(areaId).get
      val users = (request.currentUser.admin, request.currentUser.groupAdmin, selectedArea.id == Area.allArea.id) match {
        case (true, _, false) => userService.byArea(areaId)
        case (true, _, true) => userService.byAreas(request.currentUser.areas)
        case (false, true, _) => userService.byGroupIds(request.currentUser.groupIds)
        case _ =>
          eventService.warn("ALL_USER_INCORRECT_SETUP", "Erreur d'accès aux utilisateurs")
          List()
      }
      val applications = applicationService.allByArea(selectedArea.id, anonymous = true)
      val groups: List[UserGroup] = (request.currentUser.admin, request.currentUser.groupAdmin, selectedArea.id == Area.allArea.id) match {
        case (true, _, false) => groupService.allGroupByAreas(List[UUID](areaId))
        case (true, _, true) => groupService.allGroupByAreas(request.currentUser.areas)
        case (false, true, _) => groupService.byIds(request.currentUser.groupIds)
        case _ =>
          eventService.warn("ALL_USER_INCORRECT_SETUP", "Erreur d'accès aux groupes")
          List()
      }
      eventService.info("ALL_USER_SHOWED", "Visualise la vue des utilisateurs")
      Ok(views.html.allUsers(request.currentUser)(groups, users, applications, selectedArea, configuration.underlying.getString("geoplus.host")))
    }
  }

  def allCSV(areaId: java.util.UUID): Action[AnyContent] = loginAction { implicit request: RequestWithUserData[AnyContent] =>
    asAdminWhoSeesUsersOfArea(areaId) { () =>
      "ALL_USER_CSV_UNAUTHORIZED" -> "Accès non autorisé à l'export utilisateur"
    } { () =>
      val area = Area.fromId(areaId).get
      val users = if (areaId == Area.allArea.id) userService.byAreas(request.currentUser.areas)
      else userService.byArea(areaId)
      val groups = groupService.allGroupByAreas(request.currentUser.areas)
      eventService.info("ALL_USER_CSV_SHOWED", "Visualise le CSV de tous les zones de l'utilisateur")

      def userToCSV(user: User): String = {
        List[String](
          user.id.toString,
          user.name,
          user.qualite,
          user.email,
          user.creationDate.toString("dd-MM-YYYY-HHhmm", new Locale("fr")),
          if (user.helper) "Aidant" else " ",
          if (user.instructor) "Instructeur" else " ",
          if (user.groupAdmin) "Responsable" else " ",
          if (user.expert) "Expert" else " ",
          if (user.admin) "Admin" else " ",
          if (user.disabled) "Désactivé" else " ",
          user.communeCode,
          user.areas.flatMap(Area.fromId).map(_.name).mkString(","),
          user.groupIds.flatMap(id => groups.find(_.id == id)).map(_.name).mkString(","),
          if (user.cguAcceptationDate.nonEmpty) "CGU Acceptées" else "",
          if (user.newsletterAcceptationDate.nonEmpty) "Newsletter Acceptée" else ""
        ).mkString(";")
      }

      val headers = List[String]("Id", csv.USER_NAME_HEADER_PREFIX, csv.USER_QUALITY_HEADER_PREFIX,
        csv.USER_EMAIL_HEADER_PREFIX, "Création", "Aidant", csv.INSTRUCTOR_HEADER_PREFIX,
        csv.GROUP_MANAGER_HEADER_PREFIX, "Expert", "Admin", "Actif", "Commune INSEE", csv.TERRITORY_HEADER_PREFIX,
        csv.GROUP_NAME_HEADER_PREFIX, "CGU", "Newsletter").mkString(";")
      val csvContent = (List(headers) ++ users.map(userToCSV)).mkString("\n")
      val date = DateTime.now(Time.dateTimeZone).toString("dd-MMM-YYY-HHhmm", new Locale("fr"))

      Ok(csvContent).withHeaders("Content-Disposition" -> s"""attachment; filename="aplus-$date-users-${area.name.replace(" ", "-")}.csv"""")
    }
  }

  def editUser(userId: UUID): Action[AnyContent] = loginAction { implicit request: RequestWithUserData[AnyContent] =>
    asAdmin { () =>
      "VIEW_USER_UNAUTHORIZED" -> s"Accès non autorisé pour voir $userId"
    } { () =>
      userService.byIdCheckDisabled(userId, includeDisabled = true) match {
        case None =>
          eventService.error("USER_NOT_FOUND", s"L'utilisateur $userId n'existe pas")
          NotFound("Nous n'avons pas trouvé cet utilisateur")
        case Some(user) if user.canBeEditedBy(request.currentUser) =>
          val form = userForm(Time.dateTimeZone).fill(user)
          val groups = groupService.allGroups
          val unused = not(isAccountUsed(user))
          val Token(tokenName, tokenValue) = CSRF.getToken.get
          eventService.info("USER_SHOWED", "Visualise la vue de modification l'utilisateur ", user = Some(user))
          Ok(views.html.editUser(request.currentUser, request.currentArea)(form, userId, groups, unused, tokenName = tokenName, tokenValue = tokenValue))
        case _ =>
          eventService.warn("VIEW_USER_UNAUTHORIZED", s"Accès non autorisé pour voir $userId")
          Unauthorized("Vous n'avez pas le droit de faire ça")
      }
    }
  }

  def isAccountUsed(user: User): Boolean = {
    applicationService.allForUserId(userId = user.id, anonymous = false).nonEmpty
  }

  def deleteUnusedUserById(userId: UUID): Action[AnyContent] = loginAction { implicit request =>
    withUser(userId, includeDisabled = true) { user: User =>
      asAdminOfUserZone(user) { () =>
        "DELETE_USER_UNAUTHORIZED" -> s"Suppression de l'utilisateur $userId refusée."
      } { () =>
        if (isAccountUsed(user)) {
          eventService.error(code = "USER_IS_USED", description = s"Le compte ${user.id} est utilisé.")
          Unauthorized("User is not unused.")
        } else {
          userService.deleteById(userId)
          val path = "/" + controllers.routes.UserController.all(Area.allArea.id).relativeTo("/")
          Redirect(path, 303)
        }
      }
    }
  }

  def editUserPost(userId: UUID): Action[AnyContent] = loginAction { implicit request =>
    asAdmin { () =>
      "POST_EDIT_USER_UNAUTHORIZED" -> s"Accès non autorisé à modifier $userId"
    } { () =>
      userForm(Time.dateTimeZone).bindFromRequest.fold(
        formWithErrors => {
          val groups = groupService.allGroups
          eventService.error("ADD_USER_ERROR", s"Essai de modification de l'tilisateur $userId avec des erreurs de validation")
          BadRequest(views.html.editUser(request.currentUser, request.currentArea)(formWithErrors, userId, groups))
        }, updatedUser => {
          withUser(updatedUser.id, includeDisabled = true) { user: User =>
            if (!user.canBeEditedBy(request.currentUser)) {
              eventService.warn("POST_EDIT_USER_UNAUTHORIZED", s"Accès non autorisé à modifier $userId")
              Unauthorized("Vous n'avez pas le droit de faire ça")
            } else if (userService.update(updatedUser)) {
              eventService.info("EDIT_USER_DONE", s"Utilisateur $userId modifié", user = Some(updatedUser))
              Redirect(routes.UserController.all(Area.allArea.id)).flashing("success" -> "Utilisateur modifié")
            } else {
              val form = userForm(Time.dateTimeZone).fill(updatedUser).withGlobalError("Impossible de mettre à jour l'utilisateur $userId (Erreur interne)")
              val groups = groupService.allGroups
              eventService.error("EDIT_USER_ERROR", "Impossible de modifier l'utilisateur dans la BDD", user = Some(updatedUser))
              InternalServerError(views.html.editUser(request.currentUser, request.currentArea)(form, userId, groups))
            }
          }
        }
      )
    }
  }

  def importUsers: Action[AnyContent] = loginAction { implicit request =>
    asAdmin { () =>
      "IMPORT_GROUP_UNAUTHORIZED" -> "Accès non autorisé pour importer les utilisateurs"
    } { () =>
      csv.sectionsForm.bindFromRequest.fold({ missFilledForm =>
        val cleanedForm = missFilledForm.copy(data = missFilledForm.data.filter({ case (_, v) => v.nonEmpty }))
        BadRequest(views.html.reviewUsersImport(request.currentUser, request.currentArea)(cleanedForm))
      }, { sections =>
        if (sections.isEmpty) {
          val form = csv.sectionsForm.fill(sections).withGlobalError("Action impossible, il n'y a aucun utilisateur à ajouter.")
          BadRequest(views.html.reviewUsersImport(request.currentUser, request.currentArea)(form))
        } else {
          val toInsert: List[(UserGroup, List[User])] = sections.map(section => csv.prepareSection(section, request.currentUser))
          val usersToInsert: List[User] = toInsert.flatMap(_._2)
          val groupsToInsert: List[UserGroup] = toInsert.map(_._1)

          val insertResult = groupsToInsert.filterNot(group => groupService.groupById(group.id).isDefined)
            .foldLeft[Either[(String, String), Unit]](Right(()))({ case (either, group) =>
              either.fold(Left.apply, { _: Unit =>
                if (not(groupService.add(group))) {
                  Left("ADD_GROUP_ERROR" -> "Impossible d'ajouter un groupe dans la BDD.")
                } else {
                  Right(())
                }
              })
            })
          if (insertResult.isLeft) {
            val (code, description) = insertResult.left.get
            eventService.error(code, description)
            val form = csv.sectionsForm.fill(sections).withGlobalError(description)
            InternalServerError(views.html.reviewUsersImport(request.currentUser, request.currentArea)(form))
          } else if (not(userService.add(usersToInsert.filterNot(user => userService.byId(user.id).isDefined)))) {
            val description = "Impossible d'ajouter un utilisateur dans la BDD."
            eventService.error("ADD_USER_ERROR", description)
            val form = csv.sectionsForm.fill(sections).withGlobalError(description)
            InternalServerError(views.html.reviewUsersImport(request.currentUser, request.currentArea)(form))
          } else {
            eventService.info("ADD_USER_DONE", "Utilisateurs ajoutés.")
            Redirect(routes.UserController.all(request.currentArea.id)).flashing("success" -> "Utilisateurs ajoutés.")
          }
        }
      })
    }
  }

  def addPost(groupId: UUID): Action[AnyContent] = loginAction { implicit request =>
    withGroup(groupId) { group: UserGroup =>
      if (!group.canHaveUsersAddedBy(request.currentUser)) {
        eventService.warn("POST_ADD_USER_UNAUTHORIZED", "Accès non autorisé à l'admin des utilisateurs")
        Unauthorized("Vous n'avez pas le droit de faire ça")
      } else {
        implicit val area: Area = Area.fromId(group.area).get
        usersForm(Time.dateTimeZone).bindFromRequest.fold({ formWithErrors =>
          eventService.error("ADD_USER_ERROR", "Essai d'ajout d'utilisateurs avec des erreurs de validation")
          BadRequest(views.html.editUsers(request.currentUser, request.currentArea)(formWithErrors, 0, routes.UserController.addPost(groupId)))
        }, { users =>
          try {
            if (userService.add(users.map(_.copy(groupIds = List(groupId))))) {
              applicationService.addInvitedBasedOnExistingInvitedGroup(groupId, users)
              eventService.info("ADD_USER_DONE", "Utilisateurs ajouté")
              Redirect(routes.GroupController.editGroup(groupId)).flashing("success" -> "Utilisateurs ajouté")
            } else {
              val form = usersForm(Time.dateTimeZone).fill(users).withGlobalError("Impossible d'ajouté les utilisateurs (Erreur interne 1)")
              eventService.error("ADD_USER_ERROR", "Impossible d'ajouter des utilisateurs dans la BDD 1")
              InternalServerError(views.html.editUsers(request.currentUser, request.currentArea)(form, users.length, routes.UserController.addPost(groupId)))
            }
          } catch {
            case ex: PSQLException =>
              val EmailErrorPattern = """[^()@]+@[^()@.]+\.[^()@]+""".r // This didn't work in that case : """ Detail: Key \(email\)=\(([^()]*)\) already exists."""".r  (don't know why, the regex is correct)
              val errorMessage = EmailErrorPattern.findFirstIn(ex.getServerErrorMessage.toString) match {
                case Some(email) => s"Un utilisateur avec l'adresse $email existe déjà."
                case _ => "Erreur d'insertion dans la base de donnée : contacter l'administrateur."
              }
              val form = usersForm(Time.dateTimeZone).fill(users).withGlobalError(errorMessage)
              eventService.error("ADD_USER_ERROR", s"Impossible d'ajouter des utilisateurs dans la BDD : ${ex.getServerErrorMessage}")
              BadRequest(views.html.editUsers(request.currentUser, request.currentArea)(form, users.length, routes.UserController.addPost(groupId)))
          }
        })
      }
    }
  }

  def showCGU(): Action[AnyContent] = loginAction { implicit request =>
    eventService.info("CGU_SHOWED", "CGU visualisé")
    Ok(views.html.showCGU(request.currentUser, request.currentArea))
  }

  def validateCGU(): Action[AnyContent] = loginAction { implicit request =>
    validateCGUForm.bindFromRequest.fold({ formWithErrors =>
      eventService.error("CGU_VALIDATION_ERROR", "Erreur de formulaire dans la validation des CGU")
      BadRequest(s"Formulaire invalide, prévenez l'administrateur du service. ${formWithErrors.errors.mkString(", ")}")
    }, { case (redirectOption, newsletter, validate) =>
      if (validate) {
        userService.acceptCGU(request.currentUser.id, newsletter)
      }
      eventService.info("CGU_VALIDATED", "CGU validées")
      redirectOption match {
        case Some(redirect) =>
          Redirect(Call("GET", redirect)).flashing("success" -> "Merci d\'avoir accepté les CGU")
        case _ =>
          Redirect(routes.ApplicationController.myApplications()).flashing("success" -> "Merci d\'avoir accepté les CGU")
      }
    })
  }

  val validateCGUForm: Form[(Option[String], Boolean, Boolean)] = Form(tuple(
    "redirect" -> optional(text),
    "newsletter" -> boolean,
    "validate" -> boolean
  ))

  def add(groupId: UUID): Action[AnyContent] = loginAction { implicit request =>
    withGroup(groupId) { group: UserGroup =>
      if (!group.canHaveUsersAddedBy(request.currentUser)) {
        eventService.warn("SHOW_ADD_USER_UNAUTHORIZED", s"Accès non autorisé à l'admin des utilisateurs du groupe $groupId")
        Unauthorized("Vous n'avez pas le droit de faire ça")
      } else {
        implicit val area: Area = Area.fromId(group.area).get
        val rows = request.getQueryString("rows").map(_.toInt).getOrElse(1)
        eventService.info("EDIT_USER_SHOWED", "Visualise la vue d'ajouts des utilisateurs")
        Ok(views.html.editUsers(request.currentUser, request.currentArea)(usersForm(Time.dateTimeZone), rows, routes.UserController.addPost(groupId)))
      }
    }
  }

  def allEvents: Action[AnyContent] = loginAction { implicit request =>
    asAdmin { () =>
      "EVENTS_UNAUTHORIZED" -> "Accès non autorisé pour voir les événements"
    } { () =>
      val limit = request.getQueryString("limit").map(_.toInt).getOrElse(500)
      val userId = request.getQueryString("fromUserId").flatMap(UUIDHelper.fromString)
      val events = eventService.all(limit, userId)
      eventService.info("EVENTS_SHOWED", s"Affiche les événements")
      Ok(views.html.allEvents(request.currentUser, request.currentArea)(events, limit))
    }
  }

  def usersForm(timeZone: DateTimeZone)(implicit area: Area): Form[List[User]] = Form(
    single(
      "users" -> list(mapping(
        "id" -> optional(uuid).transform[UUID]({
          case None => UUID.randomUUID()
          case Some(id) => id
        }, {
          Some(_)
        }),
        "key" -> ignored("key"),
        "name" -> nonEmptyText.verifying(maxLength(100)),
        "qualite" -> nonEmptyText.verifying(maxLength(100)),
        "email" -> email.verifying(maxLength(200), nonEmpty),
        "helper" -> boolean,
        "instructor" -> boolean,
        "admin" -> ignored(false),
        "areas" -> ignored(List(area.id)),
        "creationDate" -> ignored(DateTime.now(timeZone)),
        "hasAcceptedCharte" -> ignored(false),
        "communeCode" -> default(nonEmptyText.verifying(maxLength(5)), "0"),
        "adminGroup" -> ignored(false),
        "disabled" -> ignored(false),
        "expert" -> ignored(false),
        "groupIds" -> default(list(uuid), List()),
        "delegations" -> ignored(Map[String, String]()),
        "cguAcceptationDate" -> optional(ignored(Time.now())),
        "newsletterAcceptationDate" -> optional(ignored(Time.now()))
      )(User.apply)(User.unapply))
    )
  )

  def userForm(timeZone: DateTimeZone): Form[User] = Form(userMapping(timeZone))

  private def userMapping(implicit timeZone: DateTimeZone): Mapping[User] = mapping(
    "id" -> optional(uuid).transform[UUID]({
      case None => UUID.randomUUID()
      case Some(id) => id
    }, {
      Some(_)
    }),
    "key" -> ignored("key"),
    "name" -> nonEmptyText.verifying(maxLength(100)),
    "qualite" -> nonEmptyText.verifying(maxLength(100)),
    "email" -> email.verifying(maxLength(200), nonEmpty),
    "helper" -> boolean,
    "instructor" -> boolean,
    "admin" -> boolean,
    "areas" -> list(uuid).verifying("Vous devez sélectionner au moins un territoire", _.nonEmpty),
    "creationDate" -> ignored(DateTime.now(timeZone)),
    "hasAcceptedCharte" -> boolean,
    "communeCode" -> default(nonEmptyText.verifying(maxLength(5)), "0"),
    "adminGroup" -> boolean,
    "disabled" -> boolean,
    "expert" -> ignored(false),
    "groupIds" -> default(list(uuid), List()),
    "delegations" -> seq(tuple(
      "name" -> nonEmptyText,
      "email" -> email
    )).transform[Map[String, String]]({
      _.toMap
    }, {
      _.toSeq
    }),
    "cguAcceptationDate" -> optional(ignored(Time.now())),
    "newsletterAcceptationDate" -> optional(ignored(Time.now()))
  )(User.apply)(User.unapply)

  def importUsersFromCSV: Action[AnyContent] = loginAction { implicit request =>
    asAdmin { () =>
      "IMPORT_USER_UNAUTHORIZED" -> "Accès non autorisé pour importer les utilisateurs"
    } { () =>
      Ok(views.html.importUsers(request.currentUser, request.currentArea)("", List.empty))
    }
  }

  def importUsersReview: Action[AnyContent] = {
    loginAction { implicit request =>
      asAdmin { () =>
        "IMPORT_GROUP_UNAUTHORIZED" -> "Accès non autorisé pour importer les utilisateurs"
      } { () =>
        csv.csvImportContentForm.bindFromRequest.fold({ _ =>
          BadRequest(views.html.importUsers(request.currentUser, request.currentArea)("", List(FormError.apply("csv-import-content", "Le champ est vide."))))
        }, { csvImportContent =>
          val data = csv.extractDataFromCSVAndMapToTreeStructure(csvImportContent)(userService, groupService)
          val filledForm = csv.sectionsForm.bind(data)
          Ok(views.html.reviewUsersImport(request.currentUser, request.currentArea)(filledForm))
        })
      }
    }
  }
}
