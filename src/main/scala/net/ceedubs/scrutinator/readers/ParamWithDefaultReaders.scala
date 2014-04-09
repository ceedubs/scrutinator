package net.ceedubs.scrutinator
package readers

import scalaz._
import org.scalatra.validation.{ FieldName, ValidationError }

trait ParamWithDefaultReaders {

  implicit def paramWithDefaultReader[I, A, S <: ValueSource](implicit reader: ParamReader[Validated, (NamedParam[ParamFromSource[Field[A], S]], I), Option[A]]): ParamReader[Validated, (NamedParam[ParamFromSource[ParamWithDefault[A], S]], I), A] = {
    ParamReader[Validated, (NamedParam[ParamFromSource[ParamWithDefault[A], S]], I), A](Function.tupled { (paramWithDefault, input) =>
      val nestedNamedParam: NamedParam[ParamFromSource[Field[A], S]] = NamedParam(paramWithDefault.name, ParamFromSource[Field[A], S](paramWithDefault.param.param))
      reader.reader.run((nestedNamedParam, input))
      .map(_.getOrElse(paramWithDefault.param.default))
    })
  }
}
