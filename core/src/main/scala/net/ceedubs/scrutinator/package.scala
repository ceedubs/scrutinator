package net.ceedubs

import scalaz.{ @@ => _, _}
import shapeless._
import shapeless.tag.@@
import org.scalatra.servlet.RichRequest
import scrutinator.ValueSource._

package object scrutinator {
  type Request = javax.servlet.http.HttpServletRequest
  type Errors = NonEmptyList[ScopedValidationFail]
  type Validated[+A] = Validation[Errors, A]
  type ValidatedOption[+A] = Validation[Errors, Option[A]]
  type ErrorsOr[+A] = Errors \/ A

  type ParamFromSource[A, S <: ValueSource] = shapeless.tag.@@[A, S]
  type QueryParam[A] = ParamFromSource[A, QueryString]
  type HeaderParam[A] = ParamFromSource[A, Headers]
  type PathParam[A] = ParamFromSource[A, Path]
  type JsonParam[A] = ParamFromSource[A, Json]

  /**
   * Tracks the position of a parameter.
   *
   * Most recent history is at the front of the list, so clients.projects[1] would be
   * `IndexC(1) :: FieldC("projects", None) :: FieldC("clients", None)`
   */
  type CursorHistory = List[Cursor]


  type QueryStringParams = Map[String, Seq[String]] @@ QueryString
  type HeaderParams = Map[String, String] @@ Headers

  implicit def reqToRichReq: Request => RichRequest = RichRequest.apply
}
