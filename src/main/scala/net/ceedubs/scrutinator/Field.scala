package net.ceedubs.scrutinator

import shapeless._

/**
 * A parameter of type A.
 * For example, a Field[Int] will result in an Int after binding.
 * That Int may be wrapped in an Option, depending on whether this
 * Field is wrapped in a RequiredParam, FieldWithDefault, etc.
 */
final case class Field[A](
  description: Option[String] = Field.Defaults.description,
  notes: Option[String] = Field.Defaults.notes,
  prettyName: Option[String] = Field.Defaults.prettyName,
  validations: Field.ParamValidations[A] = Field.Defaults.validations[A]) {

  def check(error: ParamError, errorMsg: => String)(f: A => Boolean): Field[A] = {
    val newValidation = (_: Cursor, a: A) =>
      if (f(a)) Nil else ValidationFail(error, Some(errorMsg)) :: Nil
    copy(validations = newValidation :: validations)
  }

  def required(errorMsg: NamedParam[Field[A]] => String): RequiredParam[Field[A]] = RequiredParam(this, errorMsg)

  def withDefault(default: A): FieldWithDefault[A] = FieldWithDefault(this, default)

}

final case class Fields[L <: HList](fields: L)

object Field {

  type ParamValidations[A] = List[Function2[FieldC, A, List[ValidationFail]]]

  object Defaults {
      val description: Option[String] = None
      val notes: Option[String] = None
      val prettyName: Option[String] = None
      def validations[A]: ParamValidations[A] = Nil
  }

}

final case class RequiredParam[A](
  param: A,
  errorMsg: NamedParam[A] => String)

final case class FieldWithDefault[A](
  param: Field[A],
  default: A)
