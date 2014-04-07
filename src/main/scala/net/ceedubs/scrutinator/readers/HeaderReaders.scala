package net.ceedubs.scrutinator
package readers

import scalaz._
import org.scalatra.validation.{ FieldName, ValidationError }

trait HeaderReaders {
  import HeaderReaders._
  import Param._

  implicit def headerNamedParamReader[A](implicit reader: ParamReader[ValidatedOption, (FieldKey, HeaderParams), A]): ParamReader[ValidatedOption, (NamedParam[HeaderParam[A]], Request), A] = {
    ParamReader[ValidatedOption, (NamedParam[HeaderParam[A]], Request), A](Function.tupled { (namedParam, request) =>
      val fieldKey = FieldKey(name = namedParam.name, prettyName = namedParam.param.prettyName) 
      val headers = HeaderParams(request.headers)
      reader.reader((fieldKey, headers)).flatMap { maybeA =>
        std.option.cata(maybeA)({ a =>
          val errors = namedParam.param.validations.map(_.apply(fieldKey, a)
            .map(e => ValidationError(e, FieldName(fieldKey.name)))).flatten
          std.option.toFailure(std.list.toNel(errors))(Some(a))
        }, Validation.success(None))
      }
    })
  }

  implicit val headerStringFieldReader: ParamReader[ValidatedOption, (FieldKey, HeaderParams), String] = {
    val kleisli = Kleisli[ValidatedOption, (FieldKey, HeaderParams), String](Function.tupled(
      (fieldKey, headerParams) =>
        Validation.success(headerParams.get(fieldKey.name).filterNot(_.isEmpty))))
    ParamReader.fromKleisli(kleisli)
 
  }

}

object HeaderReaders extends HeaderReaders {
  import ValueSource.Headers

  type HeaderParams = Map[String, String] @@ Headers

  object HeaderParams {
    def apply(params: Map[String, String]): HeaderParams = Tag[Map[String, String], Headers](params)
  }
}
