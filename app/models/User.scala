package models

import java.time.ZonedDateTime
import java.util.UUID

import constants.Constants
import helper.{Hash, UUIDHelper}

case class User(
    id: UUID,
    key: String,
    name: String,
    qualite: String,
    email: String,
    helper: Boolean,
    instructor: Boolean,
    // TODO: `private[models]` so we cannot check it without going through authorization
    admin: Boolean,
    // TODO: remove usage of areas to more specific AdministratedAreaIds
    // Note: `areas` is used to give "full access" to someone to multiple areas
    areas: List[UUID],
    creationDate: ZonedDateTime,
    communeCode: String,
    // If true, this person is managing the groups it is in
    // can see all users in its groups, and add new users in its groups
    // cannot modify users, only admin can.
    groupAdmin: Boolean,
    disabled: Boolean,
    expert: Boolean = false,
    groupIds: List[UUID] = Nil,
    cguAcceptationDate: Option[ZonedDateTime] = None,
    newsletterAcceptationDate: Option[ZonedDateTime] = None,
    phoneNumber: Option[String] = None,
    // If this field is non empty, then the User
    // is considered to be an observer:
    // * can see stats+deployment of all areas,
    // * can see all users,
    // * can see one user but not edit it
    observableOrganisationIds: List[Organisation.Id] = Nil,
    sharedAccount: Boolean = false
) extends AgeModel {
  def nameWithQualite = s"$name ( $qualite )"

  // TODO: put this in Authorization
  def canSeeUsersInArea(areaId: UUID): Boolean =
    (areaId == Area.allArea.id || areas.contains(areaId)) && (admin || groupAdmin)

}

object User {
  private val date = ZonedDateTime.parse("2017-11-01T00:00+01:00")

  val systemUser = User(
    UUIDHelper.namedFrom("system"),
    Hash.sha256(s"system"),
    "Système A+",
    "System A+",
    Constants.supportEmail,
    false,
    false,
    false,
    List(),
    date,
    "75056",
    false,
    disabled = true
  )

}
