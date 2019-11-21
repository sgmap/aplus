package models

import java.util.UUID

import org.joda.time.DateTime

case class UserGroup(id: UUID,
                     name: String,
                     description: Option[String],
                     inseeCode: List[String],
                     creationDate: DateTime,
                     createByUserId: UUID,
                     area: UUID,
                     organisation: Option[String] = None,
                     email: Option[String] = None) {

  def canHaveUsersAddedBy(user: User): Boolean =
    (user.groupAdmin && user.groupIds.contains(id)) || (user.admin && user.areas.contains(area))

  def organisationDeductedFromName(): Option[String] = {
    val lowerCaseName = name.toLowerCase()
    Organisation.all.find { organisation =>
      lowerCaseName.contains(organisation.shortName.toLowerCase()) || lowerCaseName.contains(organisation.name.toLowerCase())
    }.map(_.shortName)
  }

  lazy val organisationSetOrDeducted: Option[String] = organisation.orElse(organisationDeductedFromName)
}