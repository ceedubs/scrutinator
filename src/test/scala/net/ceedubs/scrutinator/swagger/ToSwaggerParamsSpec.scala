package net.ceedubs.scrutinator
package swagger

import org.scalatra.swagger.{ AllowableValues, DataType, Parameter, ParamType }
import shapeless._
import shapeless.syntax.singleton._

class ToSwaggerParamsSpec extends Spec {
  import Param._
  import ValueSource._

  "Swagger parameter conversion" should {
    "convert a list of Swagger parameters" ! prop { (intQueryParam: QueryParam[Int], stringHeaderParam: RequiredParam[HeaderParam[String]], longQueryParam: ParamWithDefault[Long, QueryString]) =>
      val fields =
      ("queryInt" ->> intQueryParam) ::
      ("headerString" ->> stringHeaderParam) ::
      ("queryLong" ->> longQueryParam) ::
      HNil

      SwaggerSupport.toSwaggerParams(fields) ==== Seq(
        Parameter(
          name = "queryInt",
          `type` = DataType.Int,
          description = intQueryParam.description,
          notes = intQueryParam.notes,
          paramType = ParamType.Query,
          defaultValue = None,
          allowableValues = AllowableValues.AnyValue,
          required = false),
        Parameter(
          name = "headerString",
          `type` = DataType.String,
          description = stringHeaderParam.param.description,
          notes = stringHeaderParam.param.notes,
          paramType = ParamType.Header,
          defaultValue = None,
          allowableValues = AllowableValues.AnyValue,
          required = true),
        Parameter(
          name = "queryLong",
          `type` = DataType.Long,
          description = longQueryParam.param.description,
          notes = longQueryParam.param.notes,
          paramType = ParamType.Query,
          defaultValue = Some(longQueryParam.default.toString),
          allowableValues = AllowableValues.AnyValue,
          required = false)
      )
    }
  }
}
