package services

import java.util.UUID

import javax.inject.Inject
import anorm.Column.nonNull
import models.{Answer, Application, User}
import play.api.db.Database
import play.api.libs.json.{Json, JsonConfiguration, JsonNaming}
import anorm._
import anorm.JodaParameterMetaData._
import extentions.Time
import org.joda.time.DateTime
import play.api.libs.json.JodaReads._
import play.api.libs.json.JodaWrites._

@javax.inject.Singleton
class ApplicationService @Inject()(db: Database, userService: UserService) {
  import extentions.Anorm._
  import extentions.JsonFormats._

  private implicit val answerReads = Json.reads[Answer]
  private implicit val answerWrite = Json.writes[Answer]

  implicit val answerListParser: anorm.Column[List[Answer]] =
    nonNull { (value, meta) =>
      val MetaDataItem(qualified, nullable, clazz) = meta
      value match {
        case json: org.postgresql.util.PGobject =>
          Right(Json.parse(json.getValue).as[List[Answer]])
        case json: String =>
          Right(Json.parse(json).as[List[Answer]])
        case _ => Left(TypeDoesNotMatch(s"Cannot convert $value: ${className(value)} to List[Answer] for column $qualified"))
      }
    }

  private val simpleApplication: RowParser[Application] = Macro.parser[Application](
    "id",
    "creation_date",
    "creator_user_name",
    "creator_user_id",
    "subject",
    "description",
    "user_infos",
    "invited_users",
    "area",
    "irrelevant",
    "answers",  // Data have been left bad migrated from answser_unsed
    "internal_id",
    "closed",
    "seen_by_user_ids",
    "usefulness",
    "closed_date",
    "expert_invited",
    "has_selected_subject",
    "category",
    "files"
  ).map(application => application.copy(creationDate = application.creationDate.withZone(Time.dateTimeZone), 
                    answers = application.answers.map(answer => answer.copy(creationDate = answer.creationDate.withZone(Time.dateTimeZone)))))
  
  private val simpleAnswer: RowParser[Answer] = Macro.parser[Answer](
    "id",
    "application_id",
    "creation_date",
    "message",
    "creator_user_id",
    "creator_user_name",
    "invited_users",
    "visible_by_helpers",
    "area",
    "declare_application_has_irrelevant",
    "user_infos",
    "files"
  ).map(answer => answer.copy(creationDate = answer.creationDate.withZone(Time.dateTimeZone)))


  def byId(id: UUID, fromUserId: UUID, anonymous: Boolean): Option[Application] = db.withConnection { implicit connection =>
    val answers = SQL("SELECT *, NULL as files FROM answer_unused WHERE application_id = {applicationId}::uuid ORDER BY creation_date ASC")
      // Hack to reintegrate answers that haven't been migrated (error in the migration SQL). Counting are wrong in general view.
      .on('applicationId -> id).as(simpleAnswer.*)
    val result = SQL("UPDATE application SET seen_by_user_ids = seen_by_user_ids || {seen_by_user_id}::uuid WHERE id = {id}::uuid RETURNING *")
      .on('id -> id,
          'seen_by_user_id -> fromUserId).as(simpleApplication.singleOpt)
       .map { application =>
         implicit def dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)
         // Hack to reintegrate answers that haven't been migrated (error in the migration SQL). Counting are wrong in general view.
         val newAnswers = (answers ++ application.answers).groupBy(_.id).map(_._2.head).toList.sortBy(_.creationDate)
         application.copy(answers = newAnswers)
    }
    if(anonymous){
      result.map(_.anonymousApplication)
    } else {
      result
    }
  }

  def openAndOlderThan(day: Int) =  db.withConnection { implicit connection =>
     SQL(s"SELECT * FROM application WHERE closed = false AND age(creation_date) > '$day days' AND expert_invited = false").as(simpleApplication.*)
  }

  def allOpenOrRecentForUserId(userId: UUID, anonymous: Boolean, referenceDate: DateTime): List[Application] = {
    db.withConnection { implicit connection =>
      val result = SQL(
        """SELECT * FROM application
          |WHERE (creator_user_id = {userId}::uuid OR invited_users ?? {userId}) AND
          |  (closed = FALSE OR DATE_PART('day', {referenceDate} - closed_date) < 30)
          |ORDER BY creation_date DESC""".stripMargin)
        .on('userId -> userId, 'referenceDate -> referenceDate).as(simpleApplication.*)
      if (anonymous) {
        result.map(_.anonymousApplication)
      } else {
        result
      }
    }
  }

  def allForUserId(userId: UUID, anonymous: Boolean) = db.withConnection { implicit connection =>
    val result = SQL("SELECT * FROM application WHERE creator_user_id = {userId}::uuid OR invited_users ?? {userId} ORDER BY creation_date DESC")
      .on('userId -> userId).as(simpleApplication.*)
    if(anonymous){
      result.map(_.anonymousApplication)
    } else {
      result
    }
  }

  def allForUserIds(userIds: List[UUID], anonymous: Boolean) = db.withConnection { implicit connection =>
    val result = SQL"SELECT * FROM application WHERE ARRAY[$userIds]::uuid[] @> ARRAY[creator_user_id]::uuid[] OR ARRAY(select jsonb_object_keys(invited_users))::uuid[] && ARRAY[$userIds]::uuid[] ORDER BY creation_date DESC".as(simpleApplication.*)
    if(anonymous){
      result.map(_.anonymousApplication)
    } else {
      result
    }
  }

  def allForCreatorUserId(creatorUserId: UUID, anonymous: Boolean) = db.withConnection { implicit connection =>
    val result = SQL("SELECT * FROM application WHERE creator_user_id = {creatorUserId}::uuid ORDER BY creation_date DESC")
      .on('creatorUserId -> creatorUserId).as(simpleApplication.*)
    if(anonymous){
      result.map(_.anonymousApplication)
    } else {
      result
    }
  }

  def allForInvitedUserId(invitedUserId: UUID, anonymous: Boolean) = db.withConnection { implicit connection =>
    val result = SQL("SELECT * FROM application WHERE invited_users ?? {invitedUserId} ORDER BY creation_date DESC")
      .on('invitedUserId -> invitedUserId).as(simpleApplication.*)
    if(anonymous){
      result.map(_.anonymousApplication)
    } else {
      result
    }
  }

  def addInvitedBasedOnExistingInvitedGroup(groupId: UUID, users: List[User]): Int = db.withConnection { implicit connection =>
    val usersIdOfTheGroup = userService.byGroupIds(groupId :: Nil).map(_.id)
    SQL"UPDATE application SET = invited_users || {invited_user}::jsonb WHERE ARRAY(select jsonb_object_keys(invited_users))::uuid[] && ARRAY[$usersIdOfTheGroup]::uuid[]"
      .on('invited_user -> Json.toJson(users.map(user => user.id -> user.name)))
      .executeUpdate()
  }

  def allByArea(areaId: UUID, anonymous: Boolean) = db.withConnection { implicit connection =>
    val result = SQL("SELECT * FROM application WHERE area = {areaId}::uuid ORDER BY creation_date DESC")
      .on('areaId -> areaId).as(simpleApplication.*)
    if(anonymous){
      result.map(_.anonymousApplication)
    } else {
      result
    }
  }

  def allForAreas(areaIds: List[UUID] , anonymous: Boolean) = db.withConnection { implicit connection =>
    val result = SQL"""SELECT * FROM application WHERE ARRAY[$areaIds]::uuid[] @> ARRAY[area]::uuid[] ORDER BY creation_date DESC""".as(simpleApplication.*)
    if(anonymous){
      result.map(_.anonymousApplication)
    } else {
      result
    }
  }

  def createApplication(newApplication: Application) = db.withConnection { implicit connection =>
    val invitedUserJson = Json.toJson(newApplication.invitedUsers.map {
      case (key, value) =>
         key.toString -> value
    })
    SQL"""
          INSERT INTO application (
            id,
            creation_date,
            creator_user_name,
            creator_user_id,
            subject,
            description,
            user_infos,
            invited_users,
            area,
            has_selected_subject,
            category,
            files
            ) VALUES (
            ${newApplication.id}::uuid,
            ${newApplication.creationDate},
            ${newApplication.creatorUserName},
            ${newApplication.creatorUserId}::uuid,
            ${newApplication.subject},
            ${newApplication.description},
            ${Json.toJson(newApplication.userInfos)},
            ${invitedUserJson},
            ${newApplication.area}::uuid,
            ${newApplication.hasSelectedSubject},
            ${newApplication.category},
            ${Json.toJson(newApplication.files)}::jsonb
          )
      """.executeUpdate() == 1
  }
  
  def add(applicationId: UUID, answer: Answer, expertInvited: Boolean = false) = db.withTransaction { implicit connection =>
    val invitedUserJson = Json.toJson(answer.invitedUsers.map {
      case (key, value) =>
        key.toString -> value
    })
    val sql = (if(answer.declareApplicationHasIrrelevant) { ", irrelevant = true " } else { "" }) +
      (if(expertInvited) { ", expert_invited = true"  } else { "" })

    SQL(
      s"""UPDATE application SET answers = answers || {answer}::jsonb,
          invited_users = invited_users || {invited_users}::jsonb $sql
          WHERE id = {id}::uuid
       """
    ).on(
      'id -> applicationId,
      'answer -> Json.toJson(answer),
      'invited_users -> invitedUserJson
    ).executeUpdate()
  }

  def close(applicationId: UUID, usefulness: Option[String], closedDate: DateTime) = db.withTransaction { implicit connection =>
    SQL(
      """
          UPDATE application SET closed = true, usefulness = {usefulness}, closed_date = {closed_date}
          WHERE id = {id}::uuid
       """
    ).on(
      'id -> applicationId,
      'usefulness -> usefulness,
      'closed_date -> closedDate
    ).executeUpdate() == 1
  }
}

