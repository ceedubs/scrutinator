package net.ceedubs.scrutinator
package readers

import scalaz._
import org.scalatra.validation.{ FieldName, ValidationError }

trait HeaderReaders {
  import HeaderReaders._
  import Param._

  implicit def headerNamedParamReader[M[+_], A](implicit reader: ParamReader[M, (FieldKey, HeaderParams), A]): ParamReader[M, (NamedParam[HeaderParam[A]], Request), A] = {
    reader.transform(_.local(Function.tupled { (param: NamedParam[HeaderParam[A]], request: Request) =>
      val fieldKey = FieldKey(name = param.name, prettyName = param.param.prettyName) 
      val headerParams = HeaderParams(request.headers)
      (fieldKey, headerParams)
    }))
  }

  implicit val headerStringFieldReader: ParamReader[ErrorsOrMaybe, (FieldKey, HeaderParams), String] = {
    val kleisli = Kleisli[ErrorsOrMaybe, (FieldKey, HeaderParams), String](Function.tupled(
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
