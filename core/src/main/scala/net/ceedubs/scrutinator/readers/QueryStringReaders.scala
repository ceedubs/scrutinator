package net.ceedubs.scrutinator
package readers

import scalaz.{ @@ => _, _ }
import scalaz.std.option._
import scalaz.std.list._
import shapeless.tag
import shapeless.tag._
import collection.generic.CanBuildFrom

trait QueryStringReaders {
  import QueryStringReaders._
  import Field._

  implicit def queryStringNamedParamReader[A](implicit reader: FieldReader[ValidatedOption, QueryStringParams, A]): ParamReader[ValidatedOption, (NamedParam[QueryParam[Field[A]]], Request), A] = {
    ParamReader.paramReader[ValidatedOption, (NamedParam[QueryParam[Field[A]]], Request), A] { case (history, (namedParam, request)) =>
      val fieldC = FieldC(name = namedParam.name, prettyName = namedParam.param.prettyName)
      val queryParams = QueryStringParams(request.multiParameters)
      reader.reader((history, (fieldC, queryParams))).flatMap { maybeA =>
        std.option.cata(maybeA)({ a =>
          val errors = namedParam.param.validations.
            map(_.apply(fieldC, a)
            .map(ScopedValidationFail(_, fieldC :: history))).
            flatten
          std.option.toFailure(std.list.toNel(errors))(Option(a))
        }, Validation.success(None))
      }
    }
  }

  implicit def queryStringFieldReader[A](implicit elReader: QueryStringElReader[A]): FieldReader[ValidatedOption, QueryStringParams, A] =
    FieldReader.reader[ValidatedOption, QueryStringParams, A] { (history, fieldC, queryParams) =>
      val nestedHistory = fieldC :: history
      val valueMaybe = queryParams.get(fieldC.name).
        flatMap(_.headOption.filterNot(_.isEmpty))
      Traverse[Option].traverse(valueMaybe)(s =>
        elReader.r.reader((nestedHistory, s)))
    }

  implicit def cbfQueryStringFieldReader[C[_], A](implicit elReader: QueryStringElReader[A], cbf: CanBuildFrom[Nothing, A, C[A]]): FieldReader[ValidatedOption, QueryStringParams, C[A]] =
    FieldReader.reader[ValidatedOption, QueryStringParams, C[A]] { (history, fieldC, queryParams) =>
      val fieldHistory = fieldC :: history
      Traverse[Option].traverseU(queryParams.get(fieldC.name))(seq =>
        (Traverse[List].traverseU(seq.zipWithIndex.toList){ case (s, index) =>
          val indexHistory = IndexC(index) :: fieldHistory
          elReader.r.reader((indexHistory, s))}
        ).map(_.to[C]))
    }
}

object QueryStringReaders extends QueryStringReaders {
  import ValueSource.QueryString

  object QueryStringParams {
    val tagger: Tagger[QueryString] = tag[QueryString]
    def apply(params: Map[String, Seq[String]]): QueryStringParams = tagger[Map[String, Seq[String]]](params)
  }
}

/**
 * Reads an individual element taken from query string parameters
 */
final case class QueryStringElReader[A](r: ParamReader[Validated, String, A]) {
  def andThenParse[B](formatDescription: String)(f: A => Validation[_, B]): QueryStringElReader[B] =
    QueryStringElReader(ParamReader.andThenParse(formatDescription, r)(f))
}

object QueryStringElReader {
  def reader[A](f: Function2[CursorHistory, String, Validated[A]]): QueryStringElReader[A] =
    QueryStringElReader[A](ParamReader.paramReader(f))

  implicit val stringQueryStringElReader: QueryStringElReader[String] =
    QueryStringElReader.reader((history, el) => Validation.success(el))

  implicit val intQueryStringElReader: QueryStringElReader[Int] =
    stringQueryStringElReader.andThenParse("integer")(std.string.parseInt)

  implicit val longQueryStringElReader: QueryStringElReader[Long] =
    stringQueryStringElReader.andThenParse("long")(std.string.parseLong)

  implicit val byteQueryStringElReader: QueryStringElReader[Byte] =
    stringQueryStringElReader.andThenParse("byte")(std.string.parseByte)

  implicit val doubleQueryStringElReader: QueryStringElReader[Double] =
    stringQueryStringElReader.andThenParse("double")(std.string.parseDouble)

  implicit val floatQueryStringElReader: QueryStringElReader[Float] =
    stringQueryStringElReader.andThenParse("float")(std.string.parseFloat)

  implicit val shortQueryStringElReader: QueryStringElReader[Short] =
    stringQueryStringElReader.andThenParse("short")(std.string.parseShort)
}
