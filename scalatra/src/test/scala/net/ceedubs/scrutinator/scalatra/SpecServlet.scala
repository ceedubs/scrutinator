package net.ceedubs.scrutinator
package scalatra

import org.scalatra._
import scalaz._

class SpecServlet extends ScalatraServlet {
  def customRenderPipeline: RenderPipeline = {
    case \/-(a) => a
    case -\/(errors) => UnprocessableEntity(errors)
  }

  override def renderPipeline = customRenderPipeline.orElse(super.renderPipeline)
}
