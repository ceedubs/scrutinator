package net.ceedubs

import scalaz._
import org.scalatra.validation.ValidationError
import org.scalatra.servlet.RichRequest

package object scrutinator {
  type Request = javax.servlet.http.HttpServletRequest
  type Errors = NonEmptyList[ValidationError]
  type ErrorsOr[+A] = ValidationNel[ValidationError, A]
  type ErrorsOrMaybe[+A] = ValidationNel[ValidationError, Option[A]]
  type MonadicErrorsOr[+A] = Errors \/ A

  implicit def reqToRichReq: Request => RichRequest = RichRequest.apply
}
