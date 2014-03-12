package net.ceedubs.scrutinator
package readers

import scalaz.{ @@ => _, _}
import scalaz.std.option.toSuccess
import org.scalatra.validation.{ FieldName, ValidationError }

trait ParamReader[P, I] {
  type O

  def read(input: I, param: P): ErrorsOr[O]

  def forParam(param: P): Kleisli[ErrorsOr, I, O] = Kleisli[ErrorsOr, I, O](input => read(input, param))
  
}

object ParamReader extends StringReaders with RequiredParamReaders {
  type Aux[T, I, O0] = ParamReader[T, I] { type O = O0 }
}

trait RequiredParamReaders {
  implicit def requiredParamReader[A, I, O0](implicit reader: ParamReader.Aux[NamedParam[A], I, Option[O0]]): ParamReader.Aux[NamedParam[RequiredParam[A]], I, O0] = new ParamReader[NamedParam[RequiredParam[A]], I] {
    type O = O0

    def read(i: I, param: NamedParam[RequiredParam[A]]) = {
      val nestedNamedParam = NamedParam[A](param.name, param.param.param) // params all the way down
      reader.read(i, nestedNamedParam).flatMap((o: Option[O0]) => toSuccess(o)(NonEmptyList(ValidationError(param.param.errorMsg(nestedNamedParam), FieldName(param.name)))))
    }
  }
}

trait StringReaders {
  import Param._

  implicit val optionalStringQueryParamReader: ParamReader.Aux[NamedParam[QueryParam[String]], Request, Option[String]] = new ParamReader[NamedParam[QueryParam[String]], Request] {
    type O = Option[String]

    def read(request: Request, param: NamedParam[QueryParam[String]]) = {
      Validation.success(request.parameters.get(param.name).filterNot(_.isEmpty))
    }
  }

}
