package net.ceedubs.scrutinator
package swagger

import net.ceedubs.scrutinator.json4s.readers._
import net.ceedubs.scrutinator.json4s.readers.JsonParam._
import shapeless._
import shapeless.syntax.singleton._
import org.scalatra.swagger.{ DataType, Model, ModelProperty }

class ToSwaggerModelSpec extends Spec {
  "Swagger model conversion" should {
    "convert a JSON object param into a model" ! prop { (intJsonParam: JsonFieldParam[Int], requiredStringJsonParam: RequiredParam[JsonFieldParam[String]], longJsonParam: JsonFieldParam[Long]) =>
      val bodyFields =
        ("JsonBody" ->> JsonObjectParam(
          ("jsonInt" ->> intJsonParam) ::
          ("requiredJsonString" ->> requiredStringJsonParam) ::
          ("jsonLong" ->> longJsonParam) ::
          HNil) ::
        HNil)

      val expected = Model(
        id = "JsonBody",
        name = "JsonBody",
        properties = List(
          "jsonInt" -> ModelProperty(
            `type` = DataType.Int,
            required = false,
            description = None),
          "requiredJsonString" -> ModelProperty(
            `type` = DataType.String,
            required = true,
            description = None),
          "jsonLong" -> ModelProperty(
            `type` = DataType.Long,
            required = false,
            description = None)))

      def convert[A](param: A)(implicit converter: SwaggerModelConverter[A]) = converter(param)

      (Map("JsonBody" -> expected), expected) ==== convert(NamedParam("JsonBody", bodyFields.get("JsonBody"))).apply(Map.empty[String, Model])
    }
  }
}
