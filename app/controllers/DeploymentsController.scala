package controllers

import java.time.OffsetDateTime

import com.gu.googleauth.{AuthAction, GoogleAuthConfig, UserIdentity}
import es.ES
import io.searchbox.client.JestClient
import logic.Deployments
import models._
import play.api.data.Forms._
import play.api.data.validation.Constraint
import play.api.data.{Form, Mapping, validation}
import play.api.libs.ws.WSClient
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

class DeploymentsController(
    controllerComponents: ControllerComponents,
    authAction: AuthAction[AnyContent],
    apiKeyAuth: ApiKeyAuth,
    val authConfig: GoogleAuthConfig,
    val wsClient: WSClient,
    ctx: Deployments.Context
)(implicit val ec: ExecutionContext)
    extends AbstractController(controllerComponents) {

  import DeploymentsController._

  val jestClient: JestClient = ctx.jestClient

  val healthcheck: Action[AnyContent] = Action { Ok("OK") }

  val index: Action[AnyContent] =
    authAction { request =>
      implicit val user: UserIdentity = request.user
      Ok(views.html.index())
    }

  def search(team: Option[String], service: Option[String], buildId: Option[String], page: Int): Action[AnyContent] =
    authAction { implicit request =>
      implicit val user: UserIdentity = request.user
      val showAdminColumn             = ctx.isAdmin(user)

      val filters = SearchFilters(
        team = team.filter(_.nonEmpty),
        service = service.filter(_.nonEmpty),
        buildId = buildId.filter(_.nonEmpty),
        environment = None,
        businessArea = None
      )
      val searchResult = ES.Deployments.search(filters, page).run(jestClient)
      Ok(views.html.deployments.search(searchResult, filters, showAdminColumn))
    }

  def create: Action[AnyContent] =
    apiKeyAuth.ApiKeyAuthAction.async { implicit request =>
      DeploymentForm.bindFromRequest.fold(
        _ =>
          Future.successful(
            BadRequest(
              """You must include at least the following form fields in your POST: 'team', 'service', 'buildId'.
                |You may also include the following fields:
                |- one or more links (e.g. links[0].title=PR, links[0].url=http://github.com/my-pr) (link title and URL must both be non-empty strings)
                |- a 'note' field containing any notes about the deployment (can be an empty string)
                |- a 'notifySlackChannel' field containing an additional Slack channel that you want to notify (#announce_change will always be notified)
                |- an 'environment' field set to either 'prod', 'uat' or 'nonprod'
                |- a 'businessArea' field set to your particular business area
                |- a 'gitSha' field with the Git SHA you're deploying (for future use)
                |""".stripMargin
            )
          ),
        data => {
          Deployments
            .createDeployment(
              deployment = Deployment(
                team = data.team,
                service = data.service,
                buildId = data.buildId,
                environment = None,
                businessArea = None,
                gitSha = None,
                timestamp = OffsetDateTime.now(),
                links = data.links.toList.flatten,
                note = data.note
              ),
              notifySlackChannel = data.notifySlackChannel
            )
            .run(ctx)
            .map(_ => Ok("ok"))
        }
      )
    }

  def delete(id: String) = authAction { request =>
    implicit val user: UserIdentity = request.user
    if (ctx.isAdmin(user)) {
      ES.Deployments.delete(id).run(ctx.jestClient) match {
        case Left(errorMessage) => Ok(s"Failed to delete $id. Error message: $errorMessage")
        case Right(_)           => Ok(s"Deleted $id")
      }
    } else
      Forbidden("Sorry, you're not cool enough")
  }

}

object DeploymentsController {

  case class DeploymentFormData(
      team: String,
      service: String,
      buildId: String,
      environment: Option[Environment],
      businessArea: Option[String],
      gitSha: Option[GitSha],
      links: Option[List[Link]],
      note: Option[String],
      notifySlackChannel: Option[String]
  )

  val environment: Mapping[Environment] =
    nonEmptyText
      .verifying(
        Constraint { str: String =>
          Environment.fromString(str) match {
            case Left(value) => validation.Invalid(value)
            case Right(_)    => validation.Valid
          }
        }
      )
      .transform(
        Environment.fromString(_).right.get,
        _.value
      )

  val gitSha: Mapping[GitSha] =
    nonEmptyText.transform(GitSha.apply, _.value)

  val DeploymentForm: Form[DeploymentFormData] =
    Form(
      mapping(
        "team"         -> nonEmptyText,
        "service"      -> nonEmptyText,
        "buildId"      -> nonEmptyText,
        "environment"  -> optional(environment),
        "businessArea" -> optional(nonEmptyText),
        "gitSha"       -> optional(gitSha),
        "links" -> optional(
          list(
            mapping(
              "title" -> nonEmptyText,
              "url"   -> nonEmptyText
            )(Link.apply)(Link.unapply)
          )
        ),
        "note"               -> optional(text),
        "notifySlackChannel" -> optional(nonEmptyText)
      )(DeploymentFormData.apply)(DeploymentFormData.unapply)
    )

}
