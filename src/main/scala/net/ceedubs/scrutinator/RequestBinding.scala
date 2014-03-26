package net.ceedubs.scrutinator

import shapeless._
import org.scalatra.validation.ValidationError
import scalaz._

object RequestBinding {

  def bindFromRequest[L <: HList](fields: L, request: Request)(implicit fieldBinder: FieldBinder[L, Request]): Errors \/ fieldBinder.R =
    fieldBinder(fields).run(request).disjunction

}
