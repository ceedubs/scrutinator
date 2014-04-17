package net.ceedubs.scrutinator
package scalatra

import scalaz._
import shapeless._
import org.scalatra.validation.{ FieldName, ValidationError, ValidationFail => ScalatraValidationFail }

trait ScalatraSupport {
  def validator[L <: HList](fields: L)(implicit binder: FieldBinder[L, Request]): Kleisli[ValidationErrorsOr, Request, binder.R] = ScalatraSupport.validator(fields)
}

object ScalatraSupport {

  def validator[L <: HList](fields: L)(implicit binder: FieldBinder[L, Request]): Kleisli[ValidationErrorsOr, Request, binder.R] =
  RequestBinding.fieldBinder(fields).
  mapK[ValidationErrorsOr, binder.R](_.leftMap(
    _.map(toValidationError)))

  def toValidationError(v: ScopedValidationFail): ValidationError = {
    val fieldName = v.fieldName
    ValidationError(
      message = v.failure.msg.getOrElse(s"${fieldName.getOrElse("value")} is invalid"),
      field = v.fieldName.map(FieldName(_)),
      code = Some(org.scalatra.validation.ValidationFail),
      args = Seq.empty)
  }
}
