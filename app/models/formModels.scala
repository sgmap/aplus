package models

import java.util.UUID

import play.api.data.Form
import play.api.data.Forms.{boolean, email, mapping, nonEmptyText, optional, text}
import play.api.data.validation.Constraints.maxLength

object formModels {

  case class ApplicationFormData(
      subject: String,
      description: String,
      usagerPrenom: String,
      usagerNom: String,
      usagerBirthDate: String,
      usagerOptionalInfos: Map[String, String],
      users: List[UUID],
      organismes: List[String],
      category: Option[String],
      selectedSubject: Option[String],
      signature: Option[String],
      mandatType: String,
      mandatDate: String,
      linkedMandat: Option[UUID]
  )

  case class AnswerFormData(
      message: String,
      applicationIsDeclaredIrrelevant: Boolean,
      usagerOptionalInfos: Map[String, String],
      privateToHelpers: Boolean,
      signature: Option[String]
  )

  case class InvitationFormData(
      message: String,
      invitedUsers: List[UUID],
      invitedGroups: List[UUID],
      privateToHelpers: Boolean
  )

  case class UserFormData(
      user: User,
      line: Int,
      alreadyExists: Boolean,
      alreadyExistingUser: Option[User] = None,
      isInMoreThanOneGroup: Option[Boolean] = None
  )

  case class UserGroupFormData(
      group: UserGroup,
      users: List[UserFormData],
      alreadyExistsOrAllUsersAlreadyExist: Boolean,
      doNotInsert: Boolean,
      alreadyExistingGroup: Option[UserGroup] = None
  )

  // TOOD : rename Data -> FormData
  case class CSVImportData(csvLines: String, areaIds: List[UUID], separator: Char)

  final case class ValidateSubscriptionForm(
      redirect: Option[String],
      newsletter: Boolean,
      validate: Boolean,
      firstName: Option[String],
      lastName: Option[String],
      email: String,
      qualite: String,
      phoneNumber: Option[String],
      sharedAccountName: Option[String]
  )

  object ValidateSubscriptionForm {

    def validate(user: User): Form[ValidateSubscriptionForm] = Form(
      mapping(
        "redirect" -> optional(text),
        "newsletter" -> boolean,
        "validate" -> boolean,
        "firstName" -> optional(nonEmptyText.verifying(maxLength(100))),
        "lastName" -> optional(nonEmptyText.verifying(maxLength(100))),
        "email" -> email,
        "qualite" -> nonEmptyText.verifying(maxLength(100)),
        "phoneNumber" -> optional(nonEmptyText.verifying(maxLength(40))),
        "sharedAccountName" -> optional(nonEmptyText.verifying(maxLength(100)))
      )(ValidateSubscriptionForm.apply)(ValidateSubscriptionForm.unapply)
        .verifying(
          "Le prénom est requis",
          form => if (!user.sharedAccount) form.firstName.map(_.trim).exists(_.nonEmpty) else true
        )
        .verifying(
          "Le nom est requis",
          form => if (!user.sharedAccount) form.lastName.map(_.trim).exists(_.nonEmpty) else true
        )
        .verifying(
          "Le nom du groupe partagé est requis",
          form =>
            if (user.sharedAccount) form.sharedAccountName.map(_.trim).exists(_.nonEmpty) else true
        )
    )

  }

}
