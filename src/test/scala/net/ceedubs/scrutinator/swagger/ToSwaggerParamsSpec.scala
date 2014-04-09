package net.ceedubs.scrutinator
package swagger

import org.scalatra.swagger.{ AllowableValues, DataType, Parameter, ParamType }
import net.ceedubs.scrutinator.json4s.readers._
import shapeless._
import shapeless.syntax.singleton._


class ToSwaggerParamsSpec extends Spec {
  import Field._
  import ValueSource._

  "Swagger parameter conversion" should {
    "convert a list of Swagger parameters" ! prop { (intQueryParam: QueryParam[Field[Int]], stringHeaderParam: HeaderParam[RequiredParam[Field[String]]], longQueryParam: QueryParam[ParamWithDefault[Long]], intJsonParam: Field[Int], stringJsonParam: Field[String], longJsonParam: Field[Long]) =>

      val fields =
        ("queryInt" ->> intQueryParam) ::
        ("headerString" ->> stringHeaderParam) ::
        ("queryLong" ->> longQueryParam) ::
        ("body" ->> JsonBody(SwaggerModel(
          modelId = "JsonBody",
          fields =
            ("jsonInt" ->> intJsonParam) ::
            ("jsonString" ->> stringJsonParam) ::
            ("jsonLong" ->> longJsonParam) :: HNil))) ::
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
          name = "body",
          `type` = DataType("JsonBody"),
          description = None,
          notes = None,
          paramType = ParamType.Body,
          defaultValue = None,
          allowableValues = AllowableValues.AnyValue,
          required = false)
      )

      def convertModel[A](param: A)(implicit converter: SwaggerModelConverter[A]) = converter(param)
      // code duplicated as a workaround for compiler crash
      val bodyModel = SwaggerModel(
          modelId = "JsonBody",
          fields =
            ("jsonInt" ->> intJsonParam) ::
            ("jsonString" ->> stringJsonParam) ::
            ("jsonLong" ->> longJsonParam) :: HNil)
      val expectedModels = Map(
        ModelId("JsonBody") -> convertModel(bodyModel).eval(Map.empty))
      (expectedModels, expectedParams) ==== FieldListSwaggerConverter.toSwaggerParams(fields).apply(Map.empty)
    }
  }
}
