package net.ceedubs.scrutinator

import scalaz._
import shapeless._
import org.scalatra.validation.{ FieldName, ValidationError }
import ValueSource._

/**
 * A parameter of type A.
 * For example, a Param[Int] will result in an Int after binding.
 * That Int may be wrapped in an Option, depending on whether this
 * Param is wrapped in a RequiredParam, ParamWithDefault, etc.
 */
final case class Param[A](
  description: Option[String] = Param.Defaults.description,
  notes: Option[String] = Param.Defaults.notes,
  prettyName: Option[String] = Param.Defaults.prettyName,
  validations: Param.ParamValidations[A] = Param.Defaults.validations[A]) {

  def check(errorMsg: => String)(f: A => Boolean): Param[A] = {
    val newValidation = (fieldKey: FieldKey, a: A) => if (f(a)) Nil else errorMsg :: Nil
    copy(validations = newValidation :: validations)
  }

  def required(errorMsg: NamedParam[Param[A]] => String): RequiredParam[Param[A]] = RequiredParam(this, errorMsg)

  def withDefault(default: A): ParamWithDefault[A] = ParamWithDefault(this, default)

}

object ParamFromSource {
  def apply[A, S <: ValueSource](param: A): ParamFromSource[A, S] = shapeless.tag[S].apply[A](param)
}

object QueryParam {
  def apply[A](param: A): QueryParam[A] = ParamFromSource[A, QueryString](param)
}

object HeaderParam {
  def apply[A](param: A): HeaderParam[A] = ParamFromSource[A, Headers](param)
}

object PathParam {
  def apply[A](param: A): PathParam[A] = ParamFromSource[A, Path](param)
}

final case class Fields[L <: HList](fields: L)

object JsonBody {
  def apply[A](param: A): JsonBody[A] = ParamFromSource[A, Json](param)
}

object Param {

  type ParamValidations[A] = List[Function2[FieldKey, A, List[String]]]

  object Defaults {
      val description: Option[String] = None
      val notes: Option[String] = None
      val prettyName: Option[String] = None
      def validations[A]: ParamValidations[A] = Nil
  }

}

final case class FieldKey(name: String, prettyName: Option[String]) {
  def displayName: String = prettyName.getOrElse(name)
}

trait ValueSource

object ValueSource {
  sealed trait QueryString extends ValueSource
  sealed trait Headers extends ValueSource
  sealed trait Path extends ValueSource
  sealed trait Json extends ValueSource
}

final case class RequiredParam[A](
  param: A,
  errorMsg: NamedParam[A] => String)

final case class ParamWithDefault[A](
  param: Param[A],
  default: A)

final case class NamedParam[A](name: String, param: A)

@annotation.implicitNotFound("${K} is not a supported type for a field name.")
trait NamedParamConverter[K] {
  def asNamedParam[A](param: A): NamedParam[A]
}

object NamedParamConverter {
  implicit def converter[K <: String, A](implicit w: Witness.Aux[K]): NamedParamConverter[K] = new NamedParamConverter[K] {
    def asNamedParam[A](param: A) = NamedParam[A](w.value, param)
  }
}
