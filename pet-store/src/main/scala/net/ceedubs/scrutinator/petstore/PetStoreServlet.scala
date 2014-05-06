package net.ceedubs.scrutinator.petstore

import org.scalatra._
import org.scalatra.swagger._
import org.scalatra.json._
import org.scalatra.validation.ValidationError
import scalate.ScalateSupport
import org.fusesource.scalate.{ TemplateEngine, Binding }
import org.fusesource.scalate.layout.DefaultLayoutStrategy
import javax.servlet.http.HttpServletRequest
import collection.mutable
import scalaz._
import org.json4s.{ DefaultFormats, Formats }
import net.ceedubs.scrutinator.scalatra._
import net.ceedubs.scrutinator.swagger._

abstract class PetStoreServlet(protected implicit val swagger: Swagger) extends ScalatraServlet with SwaggerSupport with ScalatraSupport with ScrutinatorSwaggerSupport {

  protected def renderResult[A](a: A)(implicit renderer: ResponseRenderer[A]): ActionResult = renderer(a)

  protected def renderer[M[+_], A, B](k: Kleisli[M, A, B])(implicit r: ResponseRenderer[M[B]]): Kleisli[Id.Id, A, ActionResult] =
    k.mapK[Id.Id, ActionResult](renderResult)

  notFound {
    // remove content type in case it was set through an action
    contentType = null
    // Try to render a ScalateTemplate if no route matched
  }
}
