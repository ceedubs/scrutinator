package net.ceedubs.scrutinator
package swagger

import org.scalatra.swagger.{ AllowableValues, DataType, Parameter, ParamType }
import net.ceedubs.scrutinator.json4s.readers._
import net.ceedubs.scrutinator.json4s.readers.JsonParam._
import shapeless._
import shapeless.syntax.singleton._

class ToSwaggerParamsSpec extends Spec {
  import Param._
  import ValueSource._

  "Swagger parameter conversion" should {
    "convert a list of Swagger parameters" ! prop { (intQueryParam: QueryParam[Int], stringHeaderParam: RequiredParam[HeaderParam[String]], longQueryParam: ParamWithDefault[Long, QueryString], intJsonParam: JsonFieldParam[Int], stringJsonParam: JsonFieldParam[String], longJsonParam: JsonFieldParam[Long]) =>
      val fields =
      ("queryInt" ->> intQueryParam) ::
      ("headerString" ->> stringHeaderParam) ::
      ("queryLong" ->> longQueryParam) ::
      ("JsonBody" ->> JsonObjectParam(
        ("jsonInt" ->> intJsonParam) ::
        ("jsonString" ->> stringJsonParam) ::
        ("jsonLong" ->> longJsonParam ::
        HNil))) ::
      HNil

      val expectedParams = Seq(
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
          required = false),
        Parameter(
          name = "JsonBody",
          `type` = DataType("JsonBody"),
          description = None,
          notes = None,
          paramType = ParamType.Body,
          defaultValue = None,
          allowableValues = AllowableValues.AnyValue,
          required = false)
      )

      def convertModel[A](param: A)(implicit converter: SwaggerModelConverter[A]) = converter(param)

      val expectedModels = Map(
        "JsonBody" -> convertModel(NamedParam("JsonBody", fields.get("JsonBody"))).eval(Map.empty))
      (expectedModels, expectedParams) ==== SwaggerSupport.toSwaggerParams(fields).apply(Map.empty)
    }
  }
}
