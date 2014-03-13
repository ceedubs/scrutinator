package net.ceedubs.scrutinator
package readers

import scalaz.{ @@ => _, _}
import shapeless.tag
import shapeless.tag.@@

trait QueryStringReaders {
  import QueryStringReaders._
  import Param._

  implicit def queryStringNamedParamReader[M[+_], A](implicit reader: ParamReader[M, (FieldKey, QueryStringParams), A]): ParamReader[M, (NamedParam[QueryParam[A]], Request), A] = {
    reader.transform(_.local(Function.tupled { (param: NamedParam[QueryParam[A]], request: Request) =>
      val fieldKey = FieldKey(name = param.name, prettyName = param.param.prettyName) 
      val queryParams = QueryStringParams(request.parameters)
      (fieldKey, queryParams)
    }))
  }

  implicit val queryStringStringFieldReader: ParamReader[ErrorsOrMaybe, (FieldKey, QueryStringParams), String] = {
    val kleisli = Kleisli[ErrorsOrMaybe, (FieldKey, QueryStringParams), String](Function.tupled(
      (fieldKey, queryParams) =>
        Validation.success(queryParams.get(fieldKey.name).filterNot(_.isEmpty))))
    ParamReader.fromKleisli(kleisli)
 
  }
}

object QueryStringReaders extends QueryStringReaders {
  import ValueSource.QueryString

  type QueryStringParams = Map[String, String] @@ QueryString

  object QueryStringParams {
    def apply(params: Map[String, String]): QueryStringParams = tag[QueryString].apply[Map[String, String]](params)
  }
}
