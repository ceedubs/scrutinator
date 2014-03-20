package net.ceedubs.scrutinator

import scalaz._
import shapeless._
import org.scalatra.validation.{ FieldName, ValidationError }

/**
  A: the value type
  S: source (QueryString, Headers, etc)
*/
case class Param[A, S <: ValueSource](
  description: Option[String] = Param.Defaults.description,
  prettyName: Option[String] = Param.Defaults.prettyName,
  validations: Param.ParamValidations[A] = Param.Defaults.validations[A]) {

  type Self = Param[A, S]

  def check(errorMsg: => String)(f: A => Boolean): Self = {
    val newValidation = (fieldKey: FieldKey, a: A) => if (f(a)) Nil else errorMsg :: Nil
    copy(validations = newValidation :: validations)
  }

  def required(errorMsg: NamedParam[Self] => String): RequiredParam[Self] = RequiredParam(this, errorMsg)

  def withDefault(default: A): ParamWithDefault[A, S] = ParamWithDefault(this, default)

}

object Param {
  import ValueSource._

  type ParamValidations[A] = List[Function2[FieldKey, A, List[String]]]

  object Defaults {
      val description: Option[String] = None
      val prettyName: Option[String] = None
      def validations[A]: ParamValidations[A] = Nil
  }

  type QueryParam[A] = Param[A, QueryString]
  type HeaderParam[A] = Param[A, Headers]

  def queryParam[A](
      description: Option[String] = Defaults.description,
      prettyName: Option[String] = Defaults.prettyName,
      validations: ParamValidations[A] = Defaults.validations[A]): QueryParam[A] = {

    Param[A, QueryString](description, prettyName, validations)
  }

  def headerParam[A](
      description: Option[String] = Defaults.description,
      prettyName: Option[String] = Defaults.prettyName,
      validations: ParamValidations[A] = Defaults.validations[A]): HeaderParam[A] = {

    Param[A, Headers](description, prettyName, validations)
  }

}

final case class FieldKey(name: String, prettyName: Option[String]) {
  def displayName: String = prettyName.getOrElse(name)
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
