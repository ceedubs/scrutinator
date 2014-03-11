package net.ceedubs.scrutinator

import shapeless._
import org.scalatra.validation.ValidationError
import scalaz._

final case class OptionalQueryParam[A](
    description: Option[String] = None/*,
    validation: Kleisli[List, A, ValidationError] = Kleisli[List, A, ValidationError](_ => Nil)*/)

object RequestBinding {

  def bindFromRequest[L <: HList](fields: L, request: Request)(implicit fieldBinder: FieldBinder[L, Request]): \/[NonEmptyList[ValidationError], fieldBinder.R] = fieldBinder(fields).run(request).disjunction
}
