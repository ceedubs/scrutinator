package net.ceedubs.scrutinator
package readers

import scalaz._

trait FieldWithDefaultReaders extends FieldWithDefaultReaders0

trait FieldWithDefaultReaders0 extends FieldWithDefaultReaders1 {
  implicit def paramWithDefaultFromSourceReader[I, A, S <: ValueSource](implicit reader: ParamReader[Validated, (NamedParam[ParamFromSource[Field[A], S]], I), Option[A]]): ParamReader[Validated, (NamedParam[ParamFromSource[FieldWithDefault[A], S]], I), A] = {
    ParamReader[Validated, (NamedParam[ParamFromSource[FieldWithDefault[A], S]], I), A] { case (cursorHistory, (paramWithDefault, input)) =>
      val nestedNamedParam: NamedParam[ParamFromSource[Field[A], S]] = NamedParam(paramWithDefault.name, ParamFromSource[Field[A], S](paramWithDefault.param.param))
      reader.reader.run((cursorHistory, (nestedNamedParam, input)))
      .map(_.getOrElse(paramWithDefault.param.default))
    }
  }
}

trait FieldWithDefaultReaders1 {
  implicit def paramWithDefaultReader[I, A](implicit reader: ParamReader[Validated, (NamedParam[Field[A]], I), Option[A]]): ParamReader[Validated, (NamedParam[FieldWithDefault[A]], I), A] = {
    ParamReader[Validated, (NamedParam[FieldWithDefault[A]], I), A] { case (cursorHistory, (paramWithDefault, input)) =>
      val nestedNamedParam = NamedParam(paramWithDefault.name, paramWithDefault.param.param)
      reader.reader.run((cursorHistory, (nestedNamedParam, input)))
      .map(_.getOrElse(paramWithDefault.param.default))
    }
  }
}
