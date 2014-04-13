package net.ceedubs.scrutinator
package swagger

import net.ceedubs.scrutinator.json4s.readers._
import shapeless._
import shapeless.syntax.singleton._
import org.scalatra.swagger.{ DataType, Model, ModelProperty }

class ToSwaggerModelSpec extends Spec {
  "Swagger model conversion" should {
    "convert a JSON object param into a model" ! prop { (intJsonParam: Field[Int], requiredStringJsonParam: RequiredParam[Field[String]], longJsonParam: Field[Long]) =>
      val bodyModel = SwaggerModel(
        modelId = "JsonBody",
        fields =
          ("jsonInt" ->> intJsonParam) ::
          ("requiredJsonString" ->> requiredStringJsonParam) ::
          ("jsonLong" ->> longJsonParam) ::
          HNil)

      val expected = Model(
        id = "JsonBody",
        name = "JsonBody",
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
            description = longJsonParam.description)))

      def convert[A](param: A)(implicit converter: SwaggerModelConverter[A]) = converter(param)

      (Map(ModelId("JsonBody") -> expected), expected) ==== convert(bodyModel).apply(Map.empty)
    }
  }
}
