package models

case class SearchFilters(
    team: Option[String],
    service: Option[String],
    buildId: Option[String],
    environment: Option[Environment],
    businessArea: Option[String]
)
