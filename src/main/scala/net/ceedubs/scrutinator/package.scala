package net.ceedubs

import scalaz._
import shapeless._
import org.scalatra.validation.ValidationError
import org.scalatra.servlet.RichRequest
import scrutinator.ValueSource._

package object scrutinator {
  type Request = javax.servlet.http.HttpServletRequest
  type Errors = NonEmptyList[ValidationError]
  type Validated[+A] = ValidationNel[ValidationError, A]
  type ValidatedOption[+A] = ValidationNel[ValidationError, Option[A]]
  type ErrorsOr[+A] = Errors \/ A

  type ParamFromSource[A, S <: ValueSource] = shapeless.tag.@@[A, S]
  type QueryParam[A] = ParamFromSource[A, QueryString]
  type HeaderParam[A] = ParamFromSource[A, Headers]
  type PathParam[A] = ParamFromSource[A, Path]
  type JsonBody[A] = ParamFromSource[A, Json]

  implicit def reqToRichReq: Request => RichRequest = RichRequest.apply
}
