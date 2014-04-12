package net.ceedubs.scrutinator
package swagger

import net.ceedubs.scrutinator.scalatra._
import net.ceedubs.scrutinator.json4s._
import org.scalatra._
import org.scalatra.swagger._
import org.scalatra.test.specs2._
import shapeless._
import shapeless.syntax.singleton._

import org.scalatra.swagger.{ ApiInfo, Swagger, SwaggerSupport }
import org.scalatra.test.specs2._
import shapeless._
import shapeless.syntax.singleton._

class ScrutinatorSwaggerSupportSpec extends Spec with MutableScalatraSpec {
  import ScrutinatorSwaggerSupportSpec._

  addServlet(classOf[SwaggerSupportSpecServlet], "/*")

  "Swagger support" should {
    "register models" in {
      post("/doStuff") {
        status ==== 200
      }
    }
  }
}

object ScrutinatorSwaggerSupportSpec extends SpecHelpers {
  class SwaggerSupportSpecServlet
        extends SpecServlet
        with SwaggerSupport
        with ScrutinatorSwaggerSupport {

    override protected val applicationDescription = "SwaggerSupportSpecServlet"
    override protected val swagger = new Swagger(Swagger.SpecVersion, "1.0", ApiInfo(
      "Scrutinator",
      "Description here",
      "blah",
      "https://github.com/ceedubs/scrutinator",
      "All Rights Reserved",
      null)) {}

    val doStuffFields =
      ("queryInt" ->> QueryParam(Field[Int]())) ::
      ("headerString" ->> HeaderParam(Field[String]())) ::
      ("pathLong" ->> PathParam(Field[Long]())) ::
      ("body" ->> JsonBody(SwaggerModel(
        modelId = "JsonBody",
        fields =
          ("string" ->> Field[String]()) ::
          ("double" ->> Field[Double]()) ::
          ("boolean" ->> Field[Boolean]()) :: HNil))
      ) :: HNil
    val doStuffOp = apiOperation[Unit]("doStuff")
      .summary("Do some stuff")
      .withParams(doStuffFields)

    val doStuffExpectedModels =  Map(
      "JsonBody" -> Model(
        id = "JsonBody",
        name = "JsonBody",
        properties = List(
          "string" -> ModelProperty(
            `type` = DataType.String,
            required = false,
            description = None),
          "double" -> ModelProperty(
            `type` = DataType.Double,
            required = false,
            description = None),
          "boolean" -> ModelProperty(
            `type` = DataType.Boolean,
            required = false,
            description = None))))

    post("/doStuff", operation(doStuffOp)) {
      val registeredModelsMap = models.toMap
      if (registeredModelsMap == doStuffExpectedModels) {
        Ok(registeredModelsMap)
      } else {
        PreconditionFailed(registeredModelsMap) 
      }
    }
  }
}
