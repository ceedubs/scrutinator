package net.ceedubs.scrutinator

import shapeless._
import org.scalatra.validation.ValidationError
import scalaz._

object RequestBinding {

  def bindFromRequest[L <: HList](fields: L)(implicit fieldBinder: FieldBinder[L, Request]): Kleisli[MonadicErrorsOr, Request, fieldBinder.R] =
    fieldBinder(fields).mapK[MonadicErrorsOr, fieldBinder.R](_.disjunction)

}
