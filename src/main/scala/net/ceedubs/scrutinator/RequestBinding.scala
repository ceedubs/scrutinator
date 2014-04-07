package net.ceedubs.scrutinator

import shapeless._
import org.scalatra.validation.ValidationError
import scalaz._

object RequestBinding {

  def bindFromRequest[L <: HList](fields: L)(implicit fieldBinder: FieldBinder[L, Request]): Kleisli[ErrorsOr, Request, fieldBinder.R] =
    fieldBinder(fields).mapK[ErrorsOr, fieldBinder.R](_.disjunction)

}
