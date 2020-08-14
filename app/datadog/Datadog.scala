package datadog

import cats.data.Kleisli
import models.{Deployment, Environment}
import play.api.libs.json.Json.{JsValueWrapper, arr, obj}
import play.api.libs.json.{JsString, JsValue}
import play.api.libs.ws.{WSClient, WSResponse}

import scala.concurrent.Future

object Datadog {

  private val url = "https://api.datadoghq.com/api/v1/events"

  case class Context(wsClient: WSClient, apiKey: String)

  def sendEvent(deployment: Deployment): Kleisli[Future, Context, WSResponse] =
    Kleisli[Future, Context, WSResponse] { ctx =>
      val json = buildPayload(deployment)
      ctx.wsClient
        .url(url)
        .addQueryStringParameters("api_key" -> ctx.apiKey)
        .post(json)
    }

  /**
    * Datadog tends to use the uat/prod convention
    * rather than the nonprod/prod convention used elsewhere
    */
  def envTag(environment: Environment): String =
    s"env:${if (environment == Environment.Prod) "prod" else "uat"}"

  def buildPayload(deployment: Deployment): JsValue =
    obj(
      "title" -> s"Deployment: ${deployment.team}/${deployment.service}",
      "text"  -> s"Service [${deployment.team}/${deployment.service}] was deployed successfully.",
      "tags" -> arr(
        (
          ("shipit:deployment": JsValueWrapper) ::
            (s"team:${deployment.team}": JsValueWrapper) ::
            (s"service:${deployment.service}": JsValueWrapper) ::
            deployment.businessArea.map[JsValueWrapper](area => s"business_area:$area").toList ++
              deployment.gitSha.map[JsValueWrapper](sha => s"git_sha:${sha.value}").toList ++
            deployment.environment.map[JsValueWrapper](envTag).toList
        ): _*
      )
    )

}
