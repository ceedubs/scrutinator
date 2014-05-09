package net.ceedubs.scrutinator
package readers

import scalaz.{ @@ => _, _}
import scalaz.std.list._

trait NamedFieldReaders {
  implicit def namedFieldReader[I, A](implicit fieldReader: FieldReader[ValidatedOption, I, A]): ParamReader[ValidatedOption, (NamedParam[Field[A]], I), A] =
    ParamReader.paramReader[ValidatedOption, (NamedParam[Field[A]], I), A] { case (history, (namedParam, input)) =>
      val readerWithValidations = ParamReader.andThenCheckField(fieldReader)((nestedHistory, fieldC, a) =>
        Field.runValidations(namedParam.param, fieldC, nestedHistory, a))
      val fieldC = FieldC(namedParam.name, prettyName = namedParam.param.prettyName)
      val nestedHistory = fieldC :: history
      readerWithValidations.reader((nestedHistory, (fieldC, input)))
    }
}
