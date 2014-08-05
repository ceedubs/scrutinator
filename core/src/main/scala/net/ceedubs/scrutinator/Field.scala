package net.ceedubs.scrutinator

import scalaz._

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
  validations: Field.ParamValidations[A] = Field.Defaults.validations[A],
  allowedValues: AllowedValues[A] = Field.Defaults.allowedValues[A]) {

  def check(error: ParamError, errorMsg: => String)(f: A => Boolean): Field[A] = {
    val newValidation = (_: Cursor, a: A) =>
      if (f(a)) Nil else ValidationFail(error, Some(errorMsg)) :: Nil
    copy(validations = newValidation :: validations)
  }

  def required(errorMsg: NamedParam[Field[A]] => String = Field.Defaults.requiredErrorMsg): RequiredParam[Field[A]] = RequiredParam(this, errorMsg)

  def withDefault(default: A): FieldWithDefault[A] = FieldWithDefault(this, default)

}

object Field {

  type ParamValidations[A] = List[Function2[FieldC, A, List[ValidationFail]]]

  object Defaults {
    val description: Option[String] = None
    val notes: Option[String] = None
    val prettyName: Option[String] = None
    def validations[A]: ParamValidations[A] = Nil
    def allowedValues[A]: AllowedValues[A] = AllowedValues.any[A]
    def requiredErrorMsg[A](n: NamedParam[Field[A]]): String =
      s"${n.param.prettyName.getOrElse(n.name)} is required"
    def requiredModelErrorMsg[M](n: NamedParam[ModelField[M]]): String =
      s"${n.param.prettyName.getOrElse(n.name)} is required"
    def requiredModelCollectionErrorMsg[C[_], M](n: NamedParam[ModelCollectionField[C, M]]): String =
      s"${n.param.prettyName.getOrElse(n.name)} is required"
  }

  def runValidations[A](field: Field[A], fieldC: FieldC, history: CursorHistory, a: A): Validated[A] = {
    val userValidation = {
      val errorList = field.validations.flatMap(_.apply(fieldC, a)).map(ScopedValidationFail(_, history))
      std.option.toFailure(std.list.toNel(errorList))(())
    }

    def fail(v: ValidationFail): ErrorsOr[Unit] = \/.left[Errors, Unit](NonEmptyList(ScopedValidationFail(v, history)))

    val allowableValueDisjunction = field.allowedValues match {
      case _: AnyValue[_] => \/.right(())
      case AllowedValueList(allowed, equal) =>
        if (Foldable[NonEmptyList].element(allowed, a)(equal)) {
          \/.right(())
        } else {
          fail(ValidationFail(ParamError.NotInPermittedSet, Some(s"${fieldC.displayName} must be one of the allowed values")))
        }
      case AllowedRange(min, max, order) =>
        if (order.greaterThanOrEqual(a, min) && order.lessThanOrEqual(a, max)) \/.right(())
        else fail(ValidationFail(ParamError.OutsideRange, Some(s"${fieldC.displayName} must be within the allowed range")))
    }
    Applicative[Validated].apply2(userValidation, allowableValueDisjunction.validation)((_, _) => a)
  }

}

final case class FieldWithDefault[A](
  param: Field[A],
  default: A)

final case class ModelField[M](
    model: M,
    description: Option[String] = Field.Defaults.description,
    notes: Option[String] = Field.Defaults.notes,
    prettyName: Option[String] = Field.Defaults.prettyName) {

  def required(errorMsg: NamedParam[ModelField[M]] => String = Field.Defaults.requiredModelErrorMsg): RequiredParam[ModelField[M]] = RequiredParam(this, errorMsg)

}

final case class ModelCollectionField[C[_], M](
    model: M,
    description: Option[String] = Field.Defaults.description,
    notes: Option[String] = Field.Defaults.notes,
    prettyName: Option[String] = Field.Defaults.prettyName) {

  def required(errorMsg: NamedParam[ModelCollectionField[C, M]] => String = Field.Defaults.requiredModelCollectionErrorMsg): RequiredParam[ModelCollectionField[C, M]] = RequiredParam(this, errorMsg)

}

object CollectionField {
  final class ModelCollectionFieldBuilder[C[_]] {
    def ofModel[M](
        model: M,
        description: Option[String] = Field.Defaults.description,
        notes: Option[String] = Field.Defaults.notes,
        prettyName: Option[String] = Field.Defaults.prettyName): ModelCollectionField[C, M] =
      ModelCollectionField[C, M](model, description, notes, prettyName)
  }

  def apply[C[_]]: ModelCollectionFieldBuilder[C] = new ModelCollectionFieldBuilder[C]
}
