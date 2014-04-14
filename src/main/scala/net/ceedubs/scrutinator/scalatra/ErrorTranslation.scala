package net.ceedubs.scrutinator
package swagger

import org.scalatra.validation.{ FieldName, ValidationError, ValidationFail => ScalatraValidationFail }

object ErrorTranslation {
  def toValidationError(v: ScopedValidationFail): ValidationError = {
    val fieldName = v.fieldName
    ValidationError(
      message = v.failure.msg.getOrElse(s"${fieldName.getOrElse("value")} is invalid"),
      field = v.fieldName.map(FieldName(_)),
      code = Some(org.scalatra.validation.ValidationFail),
      args = Seq.empty)
  }

}
