package net.ceedubs.scrutinator
package swagger

import org.scalatra.swagger.{ AllowableValues, DataType, Parameter, ParamType }
import shapeless._
import shapeless.syntax.singleton._

class ToSwaggerParamsSpec extends Spec {
  import Param._

  "Swagger parameter conversion" should {
    "convert a list of Swagger parameters" ! prop { (intQueryParam: QueryParam[Int], stringHeaderParam: RequiredParam[HeaderParam[String]]) =>
      val fields =
      ("queryInt" ->> intQueryParam) ::
      ("headerString" ->> stringHeaderParam) ::
      HNil

      SwaggerSupport.toSwaggerParams(fields) ==== Seq(
        Parameter(
          name = "queryInt",
          `type` = DataType.Int,
          description = intQueryParam.description,
          notes = None,
          paramType = ParamType.Query,
          defaultValue = None,
          allowableValues = AllowableValues.AnyValue,
          required = false),
        Parameter(
          name = "headerString",
          `type` = DataType.String,
          description = stringHeaderParam.param.description,
          notes = None,
          paramType = ParamType.Header,
          defaultValue = None,
          allowableValues = AllowableValues.AnyValue,
          required = true)
      )
    }
  }
}
