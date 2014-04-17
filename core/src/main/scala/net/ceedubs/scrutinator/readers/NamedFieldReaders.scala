package net.ceedubs.scrutinator
package readers

import scalaz.{ @@ => _, _}

trait NamedFieldReaders {
  implicit def namedFieldReader[M[+_], I, A](implicit fieldReader: FieldReader[M, I, A]): ParamReader[M, (NamedParam[Field[A]], I), A] =
    ParamReader[M, (NamedParam[Field[A]], I), A] { case (history, (namedParam, input)) =>
      val fieldC = FieldC(namedParam.name, prettyName = namedParam.param.prettyName)
      fieldReader.reader((history, (fieldC, input)))
    }
}
