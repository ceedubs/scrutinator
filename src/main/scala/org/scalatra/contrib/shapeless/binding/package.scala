package org.scalatra.contrib.shapeless

import scalaz.ValidationNel
import org.scalatra.validation.ValidationError
import org.scalatra.servlet.RichRequest

package object binding {
  type Request = javax.servlet.http.HttpServletRequest
  type ErrorsOr[A] = ValidationNel[ValidationError, A]

  implicit def reqToRichReq: Request => RichRequest = RichRequest.apply
}
