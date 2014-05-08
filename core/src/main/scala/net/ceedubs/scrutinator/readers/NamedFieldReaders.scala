package net.ceedubs.scrutinator
package readers

import scalaz.{ @@ => _, _}
import scalaz.std.list._

trait NamedFieldReaders {
  implicit def namedFieldReader[I, A](implicit fieldReader: FieldReader[ValidatedOption, I, A]): ParamReader[ValidatedOption, (NamedParam[Field[A]], I), A] =
    ParamReader.paramReader[ValidatedOption, (NamedParam[Field[A]], I), A] { case (history, (namedParam, input)) =>
      val readerWithValidations = ParamReader.andThenCheckField(fieldReader)((nestedHistory, fieldC, a) =>
        std.option.toFailure(std.list.toNel(namedParam.param.validations.flatMap(f => f(fieldC, a))))(a).
        leftMap(_.map(ScopedValidationFail(_, nestedHistory)))
      )
      val fieldC = FieldC(namedParam.name, prettyName = namedParam.param.prettyName)
      val nestedHistory = fieldC :: history
      readerWithValidations.reader((nestedHistory, (fieldC, input)))
    }
}
