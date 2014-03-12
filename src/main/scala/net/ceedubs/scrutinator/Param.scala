package net.ceedubs.scrutinator

import scalaz.{ @@ => _, _}
import shapeless._
import shapeless.tag
import shapeless.tag.@@
import org.scalatra.validation.{ FieldName, ValidationError }

case class Param[A](
  description: Option[String],
  displayName: Option[String])

object Param {
  import ValueSources._

  type QueryParam[A] = Param[A] @@ QueryString
  type HeaderParam[A] = Param[A] @@ Headers

  def queryParam[A](description: Option[String] = None): QueryParam[A] = {
    tag[QueryString].apply[Param[A]](
      Param[A](description, None))
  }

  def headerParam[A](description: Option[String] = None): HeaderParam[A] = {
    tag[Headers].apply[Param[A]](
      Param[A](description, None))
  }
}

object ValueSources {
  sealed trait QueryString
  sealed trait Headers
}

case class NamedParam[A](name: String, param: A)

case class RequiredParam[A](
  param: A,
  errorMsg: NamedParam[A] => String)

@annotation.implicitNotFound("${K} is not a supported type for a field name.")
trait NamedParamConverter[K] {
  def asNamedParam[A](param: A): NamedParam[A]
}

object NamedParamConverter {
  implicit def converter[K <: String, A](implicit w: Witness.Aux[K]): NamedParamConverter[K] = new NamedParamConverter[K] {
    def asNamedParam[A](param: A) = NamedParam[A](w.value, param)
  }
}
