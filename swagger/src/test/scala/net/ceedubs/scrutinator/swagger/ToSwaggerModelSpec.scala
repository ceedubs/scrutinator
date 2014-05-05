package net.ceedubs.scrutinator
package swagger

import shapeless._
import shapeless.syntax.singleton._
import org.scalatra.swagger.{ DataType, Model => SwaggerModel, ModelProperty }

class ToSwaggerModelSpec extends Spec {
  "Swagger model conversion" should {
    "convert a JSON object param into a model" ! prop { (intJsonParam: Field[Int], requiredStringJsonParam: RequiredParam[Field[String]], longJsonParam: Field[Long], description: Option[String], booleanWithDefaultField: FieldWithDefault[Boolean], floatField: Field[Float]) =>
      val bodyModel = ModelWithId(
        id = "JsonBody",
        model = Model(
          fields =
            ("jsonInt" ->> intJsonParam) ::
            ("requiredJsonString" ->> requiredStringJsonParam) ::
            ("jsonLong" ->> longJsonParam) ::
            ("nestedModelCollection" ->> CollectionField[List].ofModel(ModelWithId(
              id = "NestedModel",
              model = Model(
               // ("booleanWithDefault" ->> booleanWithDefaultField) ::
                ("float" ->> floatField) ::
                HNil)))) ::
            HNil,
            description = description))

      val expectedNestedModel = SwaggerModel(
        id = "NestedModel",
        name = "NestedModel",
        description = None,
        properties = List(
          "float" -> ModelProperty(
            `type` = DataType.Float,
            required = false,
            description = floatField.description)))

      val expected = SwaggerModel(
        id = "JsonBody",
        name = "JsonBody",
        description = description,
        properties = List(
          "jsonInt" -> ModelProperty(
            `type` = DataType.Int,
            required = false,
            description = intJsonParam.description),
          "requiredJsonString" -> ModelProperty(
            `type` = DataType.String,
            required = true,
            description = requiredStringJsonParam.param.description),
          "jsonLong" -> ModelProperty(
            `type` = DataType.Long,
            required = false,
            description = longJsonParam.description),
          "nestedModelCollection" -> ModelProperty(
            `type` = DataType.GenList(DataType("NestedModel")),
            required = false,
            description = None)))

      def convert[A](param: A)(implicit converter: SwaggerModelConverter[A]) = converter(param)

      (Map(
        ModelId("NestedModel") -> expectedNestedModel,
        ModelId("JsonBody") -> expected),
      expected) ==== convert(bodyModel).apply(Map.empty)
    }
  }
}
