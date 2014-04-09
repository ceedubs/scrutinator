package net.ceedubs.scrutinator
package readers

import scalaz.{ @@ => _, _}
import org.scalatra.validation.{ FieldName, ValidationError }

trait RequiredParamReaders {

  implicit def requiredParamReader[I, P, A, S <: ValueSource](implicit reader: ParamReader[Validated, (NamedParam[ParamFromSource[P, S]], I), Option[A]]): ParamReader[Validated, (NamedParam[ParamFromSource[RequiredParam[P], S]], I), A] = {
    ParamReader[Validated, (NamedParam[ParamFromSource[RequiredParam[P], S]], I), A](Function.tupled { (namedParam, input) =>
      val nestedNamedParam = NamedParam[ParamFromSource[P, S]](namedParam.name, ParamFromSource[P, S](namedParam.param.param))
      reader.reader.run((nestedNamedParam, input)).flatMap { maybeA =>
        std.option.toSuccess(maybeA) {
          val nestedNoSource: NamedParam[P] = NamedParam(namedParam.name, namedParam.param.param)
          val errorMsg = namedParam.param.errorMsg(nestedNoSource)
          NonEmptyList(ValidationError(errorMsg, FieldName(namedParam.name)))
        }
      }
    })
  }
}
