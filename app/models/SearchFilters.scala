package models

import cats.data.EitherT
import cats.instances.option._
import play.api.mvc.QueryStringBindable
import play.api.mvc.QueryStringBindable._

case class SearchFilters(
    team: Option[String],
    service: Option[String],
    buildId: Option[String],
    environment: Option[Environment],
    businessArea: Option[String]
)

object SearchFilters {

  val empty: SearchFilters =
    SearchFilters(None, None, None, None, None)

  private val bindableEnv = new QueryStringBindable[Environment] {
    def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, Environment]] =
      params.get(key).flatMap(_.headOption).filter(_.nonEmpty).map(Environment.fromString)
    def unbind(key: String, value: Environment): String =
      value.value
  }

  private def eitherT[A](a: Some[Either[String, Option[A]]]): EitherT[Option, String, Option[A]] =
    EitherT(a: Option[Either[String, Option[A]]])

  /**
    * this horrible instance is required to allow us to pass SearchFilters directly to routes.
    * Play returns some strange types from QueryStringBindables hence the weird eitherT function above
    */
  implicit val queryBindable: QueryStringBindable[SearchFilters] =
    new QueryStringBindable[SearchFilters] {
      def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, SearchFilters]] =
        (
          for {
            team         <- eitherT(bindableOption(bindableString).bind("team", params))
            service      <- eitherT(bindableOption(bindableString).bind("service", params))
            buildId      <- eitherT(bindableOption(bindableString).bind("buildId", params))
            environment  <- eitherT(bindableOption(bindableEnv).bind("environment", params))
            businessArea <- eitherT(bindableOption(bindableString).bind("businessArea", params))
          } yield SearchFilters(
            team.filter(_.nonEmpty),
            service.filter(_.nonEmpty),
            buildId.filter(_.nonEmpty),
            environment,
            businessArea.filter(_.nonEmpty)
          )
        ).value

      def unbind(key: String, value: SearchFilters): String =
        List(
          bindableOption(bindableString).unbind("team", value.team),
          bindableOption(bindableString).unbind("service", value.team),
          bindableOption(bindableString).unbind("buildId", value.team),
          bindableOption(bindableString).unbind("environment", value.team),
          bindableOption(bindableString).unbind("businessArea", value.team)
        ).filter(_.trim.nonEmpty).mkString("&")
    }
}
