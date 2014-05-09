package net.ceedubs.scrutinator
package swagger

import org.scalatra.swagger.{ AllowableValues, DataType, Parameter, ParamType }
import shapeless._
import shapeless.syntax.singleton._
import scalaz.std.string._

class ToSwaggerParamsSpec extends Spec {
  import Field._
  import ValueSource._

  "Swagger parameter conversion" should {
    "convert a list of Swagger parameters" ! prop { (intQueryParam: Field[Int], stringHeaderParam: RequiredParam[Field[String]], longQueryParam: QueryParam[FieldWithDefault[Long]], doublePathParam: PathParam[RequiredParam[Field[Double]]], intJsonParam: Field[Int], stringJsonParam: Field[String], longJsonParam: Field[Long], description: Option[String]) =>

      val fields =
        ("queryInt" ->> QueryParam(intQueryParam.copy(allowedValues = AllowedValues.range(-3, 3)))) ::
        ("headerString" ->> HeaderParam(stringHeaderParam.copy(
          param = stringHeaderParam.param.copy(allowedValues = AllowedValues.anyOf("foo", "bar"))))) ::
        ("queryLong" ->> longQueryParam) ::
        ("pathDouble" ->> doublePathParam) ::
        ("body" ->> JsonParam(ModelField(
          model = ModelWithId(
            id  = "JsonBody",
            model  = Model(
              ("jsonInt" ->> intJsonParam) ::
              ("jsonNestedObject" ->> CollectionField[List].ofModel(ModelWithId(
                id = "JsonNestedObject",
                model = Model(
                  ("jsonString" ->> stringJsonParam) ::
                  ("jsonLong" ->> longJsonParam) :: HNil)))) :: HNil,
            description = description))))) ::
        HNil

      val expectedParams = Seq(
        Parameter(
          name = "queryInt",
          `type` = DataType.Int,
          description = intQueryParam.description,
          notes = intQueryParam.notes,
          paramType = ParamType.Query,
          defaultValue = None,
          allowableValues = AllowableValues.AllowableRangeValues(-3 to 3),
          required = false),
        Parameter(
          name = "headerString",
          `type` = DataType.String,
          description = stringHeaderParam.param.description,
          notes = stringHeaderParam.param.notes,
          paramType = ParamType.Header,
          defaultValue = None,
          allowableValues = AllowableValues.AllowableValuesList(List("foo", "bar")),
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
          name = "pathDouble",
          `type` = DataType.Double,
          description = doublePathParam.param.description,
          notes = doublePathParam.param.notes,
          paramType = ParamType.Path,
          defaultValue = None,
          allowableValues = AllowableValues.AnyValue,
          required = true),
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
      val nestedObjectModel = ModelWithId(
        id = "JsonNestedObject",
        model = Model(
          ("jsonString" ->> stringJsonParam) ::
          ("jsonLong" ->> longJsonParam) :: HNil))
      val bodyModel = ModelWithId(
        id  = "JsonBody",
        model  = Model(
          ("jsonInt" ->> intJsonParam) ::
          ("jsonNestedObject" ->> CollectionField[List].ofModel(ModelWithId(
            id = "JsonNestedObject",
            model = Model(
              ("jsonString" ->> stringJsonParam) ::
              ("jsonLong" ->> longJsonParam) :: HNil)))) :: HNil,
        description = description))
      val expectedModels = Map(
        ModelId("JsonBody") -> convertModel(bodyModel).eval(Map.empty),
        ModelId("JsonNestedObject") -> convertModel(nestedObjectModel).eval(Map.empty))
      (expectedModels, expectedParams) ==== FieldListSwaggerConverter.toSwaggerParams(fields).apply(Map.empty)
    }
  }
}
