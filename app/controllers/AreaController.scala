package controllers

import java.util.UUID

import actions.LoginAction
import constants.Constants
import Operators.UserOperators
import helper.UUIDHelper
import javax.inject.{Inject, Singleton}
import models.EventType.{
  AllAreaUnauthorized,
  AreaChanged,
  ChangeAreaUnauthorized,
  DeploymentDashboardUnauthorized
}
import models.{Area, Authorization, Organisation, User}
import org.webjars.play.WebJarsUtil
import play.api.mvc.InjectedController
import services.{EventService, UserGroupService, UserService}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
case class AreaController @Inject() (
    loginAction: LoginAction,
    eventService: EventService,
    userService: UserService,
    userGroupService: UserGroupService,
    configuration: play.api.Configuration
)(implicit ec: ExecutionContext, val webJarsUtil: WebJarsUtil)
    extends InjectedController
    with UserOperators {

  private lazy val areasWithLoginByKey = configuration.underlying
    .getString("app.areasWithLoginByKey")
    .split(",")
    .flatMap(UUIDHelper.fromString)

  @deprecated("You should not need area", "v0.1")
  def change(areaId: UUID) = loginAction { implicit request =>
    if (!request.currentUser.areas.contains[UUID](areaId)) {
      eventService.log(ChangeAreaUnauthorized, s"Accès à la zone $areaId non autorisé")
      Unauthorized(
        s"Vous n'avez pas les droits suffisants pour accèder à cette zone. Vous pouvez contacter l'équipe A+ : ${Constants.supportEmail}"
      )
    } else {
      eventService.log(AreaChanged, s"Changement vers la zone $areaId")
      val redirect = request
        .getQueryString("redirect")
        .map(url => Redirect(url))
        .getOrElse(Redirect(routes.ApplicationController.myApplications()))
      redirect.withSession(request.session - "areaId" + ("areaId" -> areaId.toString))
    }
  }

  def all = loginAction { implicit request =>
    if (!request.currentUser.admin && !request.currentUser.groupAdmin) {
      eventService.log(AllAreaUnauthorized, "Accès non autorisé pour voir la page des territoires")
      Unauthorized("Vous n'avez pas le droit de faire ça")
    } else {
      val userGroups = if (request.currentUser.admin) {
        userGroupService.allGroupByAreas(request.currentUser.areas)
      } else {
        userGroupService.byIds(request.currentUser.groupIds)
      }
      Ok(
        views.html
          .allArea(request.currentUser, request.rights)(Area.all, areasWithLoginByKey, userGroups)
      )
    }
  }

  private val organisationGroupingAll: List[Set[Organisation]] = (
    List(
      Set("DDFIP", "DRFIP"),
      Set("CPAM", "CRAM", "CNAM"),
      Set("CARSAT", "CNAV")
    ) :::
      List(
        "ANAH",
        "ANTS",
        "BDF",
        "CAF",
        "CCAS",
        "CDAD",
        "Département",
        "Hôpital",
        "OFPRA",
        "La Poste",
        "Mairie",
        "MDPH",
        "MFS",
        "Mission locale",
        "MSA",
        "MSAP",
        "Pôle emploi",
        "Préf",
        "Sous-Préf"
      ).map(Set(_))
  ).map(_.flatMap(id => Organisation.byId(Organisation.Id(id))))

  private val organisationGroupingFranceService: List[Set[Organisation]] = (
    List(
      Set("DDFIP", "DRFIP"),
      Set("CPAM", "CRAM", "CNAM"),
      Set("CARSAT", "CNAV"),
      Set("ANTS", "Préf")
    ) :::
      List(
        "CAF",
        "CDAD",
        "La Poste",
        "MSA",
        "Pôle emploi"
      ).map(Set(_))
  ).map(_.flatMap(id => Organisation.byId(Organisation.Id(id))))

  def deploymentDashboard = loginAction.async { implicit request =>
    asUserWithAuthorization(Authorization.canSeeDeployment) { () =>
      DeploymentDashboardUnauthorized -> "Accès non autorisé au dashboard de déploiement"
    } { () =>
      val userGroups = userGroupService.allGroups
      userService.all.map { users =>
        def usersIn(area: Area, organisationSet: Set[Organisation]): List[User] =
          for {
            group <- userGroups.filter(group =>
              group.areaIds.contains[UUID](area.id)
                && organisationSet.exists(group.organisationSetOrDeducted.contains[Organisation])
            )
            user <- users if user.groupIds.contains[UUID](group.id)
          } yield user

        val organisationGrouping =
          if (request.getQueryString("uniquement-fs").getOrElse("non") == "oui") {
            organisationGroupingFranceService
          } else {
            organisationGroupingAll
          }

        val data = for {
          area <- request.currentUser.areas.flatMap(Area.fromId).filterNot(_.name == "Demo")
        } yield {
          val organisationMap: List[(Set[Organisation], Int)] = for {
            organisations <- organisationGrouping
            users = usersIn(area, organisations)
            userSum = users.count(_.instructor)
          } yield organisations -> userSum
          (area, organisationMap, organisationMap.count(_._2 > 0))
        }

        val organisationSetToCountOfCounts: Map[Set[Organisation], Int] = {
          val organisationSetToCount: List[(Set[Organisation], Int)] = data.flatMap(_._2)
          val countsGroupedByOrganisationSet
              : Map[Set[Organisation], List[(Set[Organisation], Int)]] =
            organisationSetToCount.groupBy(_._1)
          val organisationSetToCountOfCounts: Map[Set[Organisation], Int] =
            countsGroupedByOrganisationSet.mapValues(_.map(_._2).count(_ > 0))
          organisationSetToCountOfCounts
        }

        Ok(
          views.html.deploymentDashboard(request.currentUser, request.rights)(
            data,
            organisationSetToCountOfCounts,
            organisationGrouping
          )
        )
      }
    }
  }

  def franceServiceDeploymentDashboard = loginAction.async { implicit request =>
    asUserWithAuthorization(Authorization.canSeeDeployment) { () =>
      DeploymentDashboardUnauthorized -> "Accès non autorisé au dashboard de déploiement"
    } { () =>
      Future(Ok(views.html.franceServiceDeploymentDashboard(request.currentUser, request.rights)))
    }
  }

}
