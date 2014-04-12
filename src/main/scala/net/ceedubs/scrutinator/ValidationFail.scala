package net.ceedubs.scrutinator

import scalaz._

final case class ValidationFail(error: ParamError, msg: Option[String])

final case class ScopedValidationFail(failure: ValidationFail, scope: CursorHistory)

trait ParamError

object ParamError {
  case object InvalidFormat extends ParamError
  case object InvalidSize extends ParamError
  case object Required extends ParamError
}

object ValidationFail {

  def invalidFormatNel(history: CursorHistory, expectedType: String): Errors = {
    val msg = history match {
      case ((f: FieldC) :: _) =>
        s"${f.displayName} must be a valid $expectedType"
      case (IndexC(_) :: (f: FieldC) :: _) =>
        s"each element of ${f.displayName} must be a valid $expectedType"
      case (IndexC(_) :: _) =>
        s"each element of the array must be a valid $expectedType"
      case _ =>
        s"value must be a valid $expectedType"
    }

    NonEmptyList(
      ScopedValidationFail(
        ValidationFail(ParamError.InvalidFormat, Some(msg)),
        history))
  }

  def invalidFormat(history: CursorHistory, expectedType: String): Validated[Nothing] =
    Validation.failure(invalidFormatNel(history, expectedType))
}
