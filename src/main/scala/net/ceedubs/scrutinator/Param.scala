package net.ceedubs.scrutinator

import scalaz.{ @@ => _, _}
import shapeless._
import shapeless.tag
import shapeless.tag.@@
import org.scalatra.validation.{ FieldName, ValidationError }

/**
  A: the value type
  S: source (QueryString, Headers, etc)
*/
case class Param[A, S <: ValueSource](
  description: Option[String] = Param.Defaults.description,
  prettyName: Option[String] = Param.Defaults.prettyName,
  validation: Kleisli[List, A, ValidationError] = Param.Defaults.validation[A])

object Param {
  import ValueSource._

  type ParamValidation[A] = Kleisli[List, A, ValidationError]

  object Defaults {
      val description: Option[String] = None
      val prettyName: Option[String] = None
      def validation[A]: ParamValidation[A] = Kleisli((_: A) => Nil)
  }

  type QueryParam[A] = Param[A, QueryString]
  type HeaderParam[A] = Param[A, Headers]

  def queryParam[A](
      description: Option[String] = Defaults.description,
      prettyName: Option[String] = Defaults.prettyName,
      validation: ParamValidation[A] = Defaults.validation[A]): QueryParam[A] = {

    Param[A, QueryString](description, prettyName, validation)
  }

  def headerParam[A](
      description: Option[String] = Defaults.description,
      prettyName: Option[String] = Defaults.prettyName,
      validation: ParamValidation[A] = Defaults.validation[A]): HeaderParam[A] = {

    Param[A, Headers](description, prettyName, validation)
  }

}

trait ValueSource

object ValueSource {
  sealed trait QueryString extends ValueSource
  sealed trait Headers extends ValueSource
}

case class RequiredParam[A](
  param: A,
  errorMsg: NamedParam[A] => String)

case class ParamWithDefault[A, S <: ValueSource](
  param: Param[A, S],
  default: A)

case class NamedParam[A](name: String, param: A)

@annotation.implicitNotFound("${K} is not a supported type for a field name.")
trait NamedParamConverter[K] {
  def asNamedParam[A](param: A): NamedParam[A]
}

object NamedParamConverter {
  implicit def converter[K <: String, A](implicit w: Witness.Aux[K]): NamedParamConverter[K] = new NamedParamConverter[K] {
    def asNamedParam[A](param: A) = NamedParam[A](w.value, param)
  }
}
