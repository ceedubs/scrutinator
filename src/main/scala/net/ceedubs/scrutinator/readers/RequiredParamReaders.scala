package net.ceedubs.scrutinator
package readers

import scalaz.{ @@ => _, _}
import org.scalatra.validation.{ FieldName, ValidationError }

trait RequiredParamReaders {

  implicit def requiredParamReader[I, P, A](implicit reader: ParamReader[Validated, (NamedParam[P], I), Option[A]]): ParamReader[Validated, (NamedParam[RequiredParam[P]], I), A] = {
    ParamReader[Validated, (NamedParam[RequiredParam[P]], I), A](Function.tupled { (namedParam, input) =>
      val nestedNamedParam = NamedParam[P](namedParam.name, namedParam.param.param)
      reader.reader.run((nestedNamedParam, input)).flatMap { maybeA =>
        std.option.toSuccess(maybeA) {
          val errorMsg = namedParam.param.errorMsg(nestedNamedParam)
          NonEmptyList(ValidationError(errorMsg, FieldName(namedParam.name)))
        }
      }
    })
  }
}
