package net.ceedubs.scrutinator.petstore

import org.scalatra.swagger._
import org.scalatra._
import javax.servlet.http.HttpServletResponse

object PetStoreSwagger {
  val apiInfo = ApiInfo(
    "Scrutinator Petstore",
    "This is a sample Swagger Petstore API powered by Scrutinator",
    "https://github.com/ceedubs/scrutinator/blob/master/LICENSE",
    "hello@helloreverb.com",
    "https://github.com/ceedubs/scrutinator",
    null)
}

class PetStoreSwagger extends Swagger(Swagger.SpecVersion, "0.1-SNAPSHOT", PetStoreSwagger.apiInfo)

class ResourcesApp(implicit val swagger: Swagger) extends ScalatraServlet with JacksonSwaggerBase {

  before() {
    contentType = "application/json;charset=utf-8"
  }

  //override protected def addSessionId(uri: String)(implicit response: HttpServletResponse): String = uri
}
