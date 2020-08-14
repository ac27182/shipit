package models

import java.time.OffsetDateTime

import io.circe.{Decoder, Encoder}

case class Link(title: String, url: String)

case class Deployment(
    team: String,
    service: String,
    buildId: String,
    environment: Option[Environment],
    businessArea: Option[String],
    gitSha: Option[GitSha],
    timestamp: OffsetDateTime,
    links: Seq[Link],
    note: Option[String]
)

case class GitSha(value: String) extends AnyVal

object GitSha {
  implicit val decoder: Decoder[GitSha] = Decoder.decodeString.map(GitSha.apply)
  implicit val encoder: Encoder[GitSha] = Encoder.encodeString.contramap(_.value)
}

sealed abstract class Environment(val value: String)

object Environment {

  case object Nonprod extends Environment("nonprod")
  case object Prod    extends Environment("prod")

  /**
    * I am being fairly kind when parsing environments from strings
    * as I know from experience it can be a pain to change existing builds
    */
  def fromString(env: String): Either[String, Environment] =
    env.toLowerCase match {
      case "prod" | "prd"                => Right(Prod)
      case "uat" | "nonprod" | "preprod" => Right(Nonprod)
      case _                             => Left("Bad environment. Please specify 'prod' or 'nonprod'.")
    }

  implicit val decoder: Decoder[Environment] =
    Decoder.decodeString.emap(fromString)

  implicit val encoder: Encoder[Environment] =
    Encoder.encodeString.contramap(_.value)
}
