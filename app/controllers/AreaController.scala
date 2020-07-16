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
import models.{Area, Authorization, Organisation, User, UserGroup}
import org.webjars.play.WebJarsUtil
import play.api.mvc.InjectedController
import serializers.Keys
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

  private lazy val areasWithLoginByKey: List[UUID] = configuration.underlying
    .getString("app.areasWithLoginByKey")
    .split(",")
    .flatMap(UUIDHelper.fromString)
    .toList

  def all = loginAction.async { implicit request =>
    if (!request.currentUser.admin && !request.currentUser.groupAdmin) {
      eventService.log(AllAreaUnauthorized, "Accès non autorisé pour voir la page des territoires")
      Future(Unauthorized("Vous n'avez pas le droit de faire ça"))
    } else {
      val userGroupsFuture: Future[List[UserGroup]] = if (request.currentUser.admin) {
        userGroupService.byAreas(request.currentUser.areas)
      } else {
        Future(userGroupService.byIds(request.currentUser.groupIds))
      }
      userGroupsFuture.map { userGroups =>
        Ok(
          views.html
            .allArea(request.currentUser, request.rights)(Area.all, areasWithLoginByKey, userGroups)
        )
      }
    }
  }

  private val organisationGroupingAll: List[Set[Organisation]] = {
    val groups: List[Set[Organisation]] = List(
      Set("DDFIP", "DRFIP"),
      Set("CPAM", "CRAM", "CNAM"),
      Set("CARSAT", "CNAV")
    ).map(_.flatMap(id => Organisation.byId(Organisation.Id(id))))
    val groupedSet: Set[Organisation.Id] = groups.flatMap(_.map(_.id)).toSet
    val nonGrouped: List[Organisation] =
      Organisation.all.filterNot(org => groupedSet.contains(org.id))
    groups ::: nonGrouped.map(Set(_))
  }

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
    asUserWithAuthorization(Authorization.isAdminOrObserver) { () =>
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
          if (request.getQueryString(Keys.QueryParam.uniquementFs).getOrElse("oui") == "oui") {
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
            countsGroupedByOrganisationSet.view.mapValues(_.map(_._2).count(_ > 0)).toMap
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
    asUserWithAuthorization(Authorization.isAdminOrObserver) { () =>
      DeploymentDashboardUnauthorized -> "Accès non autorisé au dashboard de déploiement"
    } { () =>
      Future(Ok(views.html.franceServiceDeploymentDashboard(request.currentUser, request.rights)))
    }
  }

}
