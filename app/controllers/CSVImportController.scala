package controllers

import java.util.UUID

import actions.LoginAction
import extentions.UUIDHelper
import extentions.Operators.{GroupOperators, UserOperators}
import forms.Models.{CSVImportData, UserFormData, UserGroupFormData}
import javax.inject.Inject
import models.{Area, Organisation, User, UserGroup}
import org.webjars.play.WebJarsUtil
import play.api.data.{Form, Mapping}
import play.api.data.Forms.{uuid, _}
import play.api.mvc.{Action, AnyContent, InjectedController}
import services.{EventService, NotificationService, UserGroupService, UserService}
import org.joda.time.DateTime
import helper.CsvHelper
import extentions.Time
import play.api.data.validation.Constraints.{maxLength, nonEmpty}

case class CSVImportController @Inject()(loginAction: LoginAction,
                                    userService: UserService,
                                    groupService: UserGroupService,
                                    notificationsService: NotificationService,
                                    eventService: EventService)(implicit val webJarsUtil: WebJarsUtil) extends InjectedController with play.api.i18n.I18nSupport with UserOperators with GroupOperators {


  val csvImportContentForm: Form[CSVImportData] = Form(
    mapping(
    "csv-lines" -> nonEmptyText,
    "area-default-ids" -> list(uuid),
    "separator" -> char.verifying("Séparateur incorrect", (value: Char) => (value == ';' || value == ','))
  )(CSVImportData.apply)(CSVImportData.unapply))

  def importUsersFromCSV: Action[AnyContent] = loginAction { implicit request =>
    asAdmin { () =>
      "IMPORT_USER_UNAUTHORIZED" -> "Accès non autorisé pour importer les utilisateurs"
    } { () =>
      Ok(views.html.importUsersCSV(request.currentUser)(csvImportContentForm))
    }
  }

  def augmentUserGroupInformation(userGroupFormData: UserGroupFormData): UserGroupFormData = {
    val userEmails = userGroupFormData.users.map(_.user.email)
    val alreadyExistingUsers = userService.byEmails(userEmails)
    val newUsersFormDataList = userGroupFormData.users.map { userDataForm =>
      alreadyExistingUsers.find(_.email == userDataForm.user.email).fold {
        userDataForm
      } { alreadyExistingUser =>
        userDataForm.copy(user = userDataForm.user.copy(id = alreadyExistingUser.id), alreadyExistingUser = Some(alreadyExistingUser))
      }
    }
    groupService.groupByName(userGroupFormData.group.name).fold {
      userGroupFormData
    } { alreadyExistingGroup =>
      userGroupFormData.copy(
        group = userGroupFormData.group.copy(id = alreadyExistingGroup.id), 
        alreadyExistingGroup = Some(alreadyExistingGroup)
      )
    }.copy(users = newUsersFormDataList) 
  }

  def userImportMapping(date: DateTime): Mapping[User] = mapping(
    "id" -> optional(uuid).transform[UUID]({
      case None => UUID.randomUUID()
      case Some(id) => id
    }, {
      Some(_)
    }),
    "key" -> ignored("key"),
    "name" -> nonEmptyText.verifying(maxLength(100)),
    "qualite" -> default(text, ""),
    "email" -> email.verifying(maxLength(200), nonEmpty),
    "helper" -> boolean,
    "instructor" -> boolean,
    "admin" -> ignored(false),
    "areas" -> list(uuid),
    "creationDate" -> ignored(date),
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
    "cguAcceptationDate" -> ignored(Option.empty[DateTime]),
    "newsletterAcceptationDate" -> ignored(Option.empty[DateTime]),
    "phone-number" -> optional(text),
  )(User.apply)(User.unapply)

  def groupImportMapping(date: DateTime): Mapping[UserGroup] = mapping(
    "id" -> optional(uuid).transform[UUID]({
      case None => UUID.randomUUID()
      case Some(id) => id
    }, {
      Some(_)
    }),
    "name" -> text(maxLength = 60),
    "description" -> optional(text),
    "insee-code" -> list(text),
    "creationDate" -> ignored(date),
    "area-ids" -> list(uuid).verifying("Vous devez sélectionner au moins 1 territoire", _.nonEmpty),
    "organisation" -> optional(text).verifying("Vous devez sélectionner une organisation dans la liste", organisation =>
      organisation.map(Organisation.fromShortName).forall(_.isDefined)
    ),
    "email" -> optional(email)
  )(UserGroup.apply)(UserGroup.unapply)

  def importUsersReviewFrom(date: DateTime): Form[List[UserGroupFormData]] = Form(
    single(
    "groups" -> list(
            mapping(
              "group" -> groupImportMapping(date),
              "users" -> list(
                  mapping(
                    "user" -> userImportMapping(date),
                    "line" -> number,
                    "alreadyExistingUser" -> ignored(Option.empty[User])
                  )(UserFormData.apply)(UserFormData.unapply)
              ),
              "alreadyExistingGroup" -> ignored(Option.empty[UserGroup])
            )(UserGroupFormData.apply)(UserGroupFormData.unapply)
        )
    )
  )

  def importUsersReview: Action[AnyContent] = {
    loginAction { implicit request =>
      asAdmin { () =>
        "IMPORT_GROUP_UNAUTHORIZED" -> "Accès non autorisé pour importer les utilisateurs"
      } { () =>
        csvImportContentForm.bindFromRequest.fold({ csvImportContentFormWithError =>
          eventService.warn(code = "CSV_IMPORT_INPUT_EMPTY", description = "Le champ d'import de CSV est vide ou le séparateur n'est pas défini.")
          BadRequest(views.html.importUsersCSV(request.currentUser)(csvImportContentFormWithError))
        }, { csvImportData =>
          val defaultAreas = csvImportData.areaIds.flatMap(Area.fromId)
          CsvHelper.csvLinesToUserGroupData(csvImportData.separator, defaultAreas, Time.now())(csvImportData.csvLines).fold({
            error: String =>
              val csvImportContentFormWithError = csvImportContentForm.fill(csvImportData).withGlobalError(error)
              eventService.warn(code = "CSV_IMPORT_FORM_ERROR", description = "Erreur de formulaire Importation")
              BadRequest(views.html.importUsersCSV(request.currentUser)(csvImportContentFormWithError))
          }, {
            case (userNotImported: List[String], userGroupDataForm: List[UserGroupFormData]) =>
              val augmentedUserGroupInformation: List[UserGroupFormData] = userGroupDataForm.map(augmentUserGroupInformation)
              val (alreadyExistingUsersErrors, filteredUserGroupInformation) = CsvHelper.filterAlreadyExistingUsersAndGenerateErrors(augmentedUserGroupInformation)

              val currentDate = Time.now()
              val formWithData = importUsersReviewFrom(currentDate)
                .fillAndValidate(filteredUserGroupInformation)

              val formWithError = if(userNotImported.nonEmpty || alreadyExistingUsersErrors.nonEmpty) {
                formWithData.withGlobalError("Certaines lignes du CSV n'ont pas pu être importé", userNotImported ++ alreadyExistingUsersErrors: _*)
              } else {
                formWithData
              }
              Ok(views.html.reviewUsersImport(request.currentUser)(formWithError))
          })
        })
      }
    }
  }

  def importUsersReviewPost: Action[AnyContent] = loginAction { implicit request =>
    asAdmin { () =>
      "IMPORT_USERS_UNAUTHORIZED" -> "Accès non autorisé pour importer les utilisateurs"
    } { () =>
      val currentDate = Time.now()
      importUsersReviewFrom(currentDate).bindFromRequest.fold({ importUsersReviewFromWithError =>
        eventService.warn(code = "IMPORT_USER_FORM_ERROR", description = "Erreur de formulaire de review")
        BadRequest(views.html.reviewUsersImport(request.currentUser)(importUsersReviewFromWithError))
      }, { userGroupDataForm: List[UserGroupFormData] =>
        val augmentedUserGroupInformation: List[UserGroupFormData] = userGroupDataForm.map(augmentUserGroupInformation)

        val groupsToInsert = augmentedUserGroupInformation
          .filter(_.alreadyExistingGroup.isEmpty)
          .map(_.group)

        groupService.add(groupsToInsert)
          .fold( { error: String =>
            val description = s"Impossible d'importer les groupes : $error"
            eventService.error("IMPORT_USER_ERROR", description)
            val formWithError = importUsersReviewFrom(currentDate)
              .fill(augmentedUserGroupInformation)
              .withGlobalError(description)
            InternalServerError(views.html.reviewUsersImport(request.currentUser)(formWithError))
        }, {  Unit =>
          groupsToInsert.foreach({ userGroup =>
            eventService.info("ADD_USER_GROUP_DONE", s"Groupe ${userGroup.id} ajouté par l'utilisateur d'id ${request.currentUser.id}")
          })
          val usersToInsert = augmentedUserGroupInformation.flatMap(_.users)
            .filter(_.alreadyExistingUser.isEmpty)
            .map(_.user)
          userService.add(usersToInsert).fold({ error: String =>
            val description = s"Impossible d'importer les utilisateurs : $error"
            eventService.error("IMPORT_USER_ERROR", description)
            val formWithError = importUsersReviewFrom(currentDate)
              .fill(augmentedUserGroupInformation)
              .withGlobalError(description)
            InternalServerError(views.html.reviewUsersImport(request.currentUser)(formWithError))
          }, { Unit =>
            usersToInsert.foreach { user =>
              notificationsService.newUser(user)
              eventService.info("ADD_USER_DONE", s"Ajout de l'utilisateur ${user.name} ${user.email}", user = Some(user))
            }
            eventService.info("IMPORT_USERS_DONE", "Utilisateurs ajoutés par l'importation")
            Redirect(routes.UserController.all(request.currentArea.id)).flashing("success" -> "Utilisateurs importés.")
          })
        })
      })
    }
  }
}
