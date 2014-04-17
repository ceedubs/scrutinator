package net.ceedubs.scrutinator
package readers

import scalaz._
import scalaz.std.option.toSuccess
import ValidationFail._

trait ParamReader[M[+_], I, O] {

  def reader: Kleisli[M, (CursorHistory, I), O]

  def transform[M2[+_], I2, O2](f: Kleisli[M, (CursorHistory, I), O] => Kleisli[M2, (CursorHistory, I2), O2]): ParamReader[M2, I2, O2] = ParamReader.fromKleisli(f(reader))

  def local[I2](f: I2 => I): ParamReader[M, I2, O] = transform(_.local {
    case (c, i2) => (c, f(i2))
  })

  def mapK[N[+_], O2](f: M[O] => N[O2]): ParamReader[N, I, O2] = transform(_.mapK(f))

}

object ParamReader extends QueryStringReaders with OptionalParamReaders
  with RequiredParamReaders with HeaderReaders
  with FieldWithDefaultReaders with NamedFieldReaders {

  def apply[M[+_], I, O](f: Function2[CursorHistory, I, M[O]]): ParamReader[M, I, O] = fromKleisli(Kleisli(f.tupled))

  def fromKleisli[M[+_], I, O](k: Kleisli[M, (CursorHistory, I), O]): ParamReader[M, I, O] = new ParamReader[M, I, O] {

    val reader = k
  }

  def andThenCheckField[I, A, B](r: FieldReader[ValidatedOption, I, A])(f: Function3[CursorHistory, FieldC, A, Validated[B]]): FieldReader[ValidatedOption, I, B] = {
    FieldReader.reader[ValidatedOption, I, B] { (history, fieldKey, input) =>
      r.reader((history, (fieldKey, input))).flatMap(maybeA =>
        std.option.cata(maybeA)(a =>
          f(history, fieldKey, a).map(Some.apply),
          Validation.success(None)))}
  }

  def andThenCheckValue[I, A, B](reader: ParamReader[Validated, I, A])(f: Function2[CursorHistory, A, Validated[B]]): ParamReader[Validated, I, B] =
    ParamReader[Validated, I, B]((history, input) =>
      reader.reader((history, input)).
      flatMap(a => f(history, a)))

  def andThenParse[I, A, B](formatDescription: String, reader: ParamReader[Validated, I, A])(f: A => Validation[_, B]): ParamReader[Validated, I, B] =
    andThenCheckValue(reader)((history, a) =>
      f(a).leftMap(_ => invalidFormatNel(history, formatDescription)))
}

object FieldReader {
  def reader[M[+_], I, O](f: Function3[CursorHistory, FieldC, I, M[O]]): FieldReader[M, I, O] =
    ParamReader[M, (FieldC, I), O] { case (history, (fieldC, i)) =>
      f(history, fieldC, i)
    }

  def parseField[I, A, B](formatDescription: String)(f: A => Validation[_, B])(implicit reader: FieldReader[ValidatedOption, I, A]): FieldReader[ValidatedOption, I, B] =
    ParamReader.andThenCheckField(reader) { (history, fieldKey, a) =>
      f(a).leftMap(_ => invalidFormatNel(history, formatDescription))
    }
}
