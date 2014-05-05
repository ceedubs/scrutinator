package net.ceedubs.scrutinator
package swagger

import ValueSource._

trait SwaggerSourceConverter[S <: ValueSource] {
  def sourceType: org.scalatra.swagger.ParamType.ParamType
}

object SwaggerSourceConverter {
  def apply[S <: ValueSource](s: org.scalatra.swagger.ParamType.ParamType): SwaggerSourceConverter[S] =
    new SwaggerSourceConverter[S] {
      val sourceType = s
    }

  implicit val headerSourceConverter: SwaggerSourceConverter[Headers] =
    SwaggerSourceConverter(org.scalatra.swagger.ParamType.Header)

  implicit val queryStringSourceConverter: SwaggerSourceConverter[QueryString] =
    SwaggerSourceConverter(org.scalatra.swagger.ParamType.Query)

  implicit val jsonBodySourceConverter: SwaggerSourceConverter[Json] =
    SwaggerSourceConverter(org.scalatra.swagger.ParamType.Body)

  implicit val pathSourceConverter: SwaggerSourceConverter[Path] =
    SwaggerSourceConverter(org.scalatra.swagger.ParamType.Path)
}
