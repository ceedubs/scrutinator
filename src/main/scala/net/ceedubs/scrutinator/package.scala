package net.ceedubs

import scalaz._
import org.scalatra.validation.ValidationError
import org.scalatra.servlet.RichRequest

package object scrutinator {
  type Request = javax.servlet.http.HttpServletRequest
  type Errors = NonEmptyList[ValidationError]
  type Validated[+A] = ValidationNel[ValidationError, A]
  type ValidatedOption[+A] = ValidationNel[ValidationError, Option[A]]
  type ErrorsOr[+A] = Errors \/ A

  implicit def reqToRichReq: Request => RichRequest = RichRequest.apply
}
