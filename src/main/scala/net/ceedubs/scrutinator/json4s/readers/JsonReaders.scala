package net.ceedubs.scrutinator
package json4s
package readers

import net.ceedubs.scrutinator.readers._
import scalaz._
import scalaz.std.list._
import shapeless._
import shapeless.record._
import org.json4s._
import org.json4s.jackson.JsonMethods
import org.scalatra.validation.{ FieldName, ValidationError }
import collection.generic.CanBuildFrom

object JsonReaders {
  sealed trait InvalidFormat
  case object InvalidFormat extends InvalidFormat
  type Parsed[+A] = InvalidFormat \/ A
  type JValueReader[A] = ParamReader[Parsed, JValue, Option[A]]

  object JValueReader {
    def reader[A](f: JValue => InvalidFormat \/ Option[A]): JValueReader[A] =
      ParamReader[Parsed, JValue, Option[A]](f)
  }
}

trait JsonReaders {
  import JsonReaders._

  implicit val stringJValueReader: JValueReader[String] =
    JValueReader.reader( _ match {
      case JString(s) => \/.right(Some(s))
      case JNothing | JNull => \/.right(None)
      case x => \/.left(InvalidFormat)
    })

  implicit val intJValueReader: JValueReader[Int] =
    JValueReader.reader( _ match {
      case JInt(x) => \/.right(Some(x.toInt))
      case JNothing | JNull => \/.right(None)
      case x => \/.left(InvalidFormat)
    })

  implicit val longJValueReader: JValueReader[Long] =
    JValueReader.reader( _ match {
      case JInt(x) => \/.right(Some(x.toLong))
      case JNothing | JNull => \/.right(None)
      case x => \/.left(InvalidFormat)
    })

  implicit val doubleJValueReader: JValueReader[Double] =
    JValueReader.reader( _ match {
      case JDouble(x) => \/.right(Some(x.toDouble))
      case JInt(x) => \/.right(Some(x.toDouble))
      case JNothing | JNull => \/.right(None)
      case x => \/.left(InvalidFormat)
    })

  implicit val floatJValueReader: JValueReader[Float] =
    JValueReader.reader( _ match {
      case JDouble(x) => \/.right(Some(x.toFloat))
      case JDecimal(x) => \/.right(Some(x.toFloat))
      case JInt(x) => \/.right(Some(x.toFloat))
      case JNothing | JNull => \/.right(None)
      case x => \/.left(InvalidFormat)
    })

  implicit val shortJValueReader: JValueReader[Short] =
    JValueReader.reader( _ match {
      case JInt(x) => \/.right(Some(x.toShort))
      case JNothing | JNull => \/.right(None)
      case x => \/.left(InvalidFormat)
    })

  implicit val booleanJValueReader: JValueReader[Boolean] =
    JValueReader.reader( _ match {
      case JBool(b) => \/.right(Some(b))
      case JNothing | JNull => \/.right(None)
      case x => \/.left(InvalidFormat)
    })

  implicit def cbfJValueReader[C[_], A](implicit entryReader: JValueReader[A], cbf: CanBuildFrom[Nothing, A, C[A]]): JValueReader[C[A]] =
    JValueReader.reader( _ match {
      case JArray(l) =>
        Traverse[List].traverseU(l)(jValue =>
          entryReader.reader(jValue).flatMap(o => std.option.toRight(o)(InvalidFormat))
        ).map(list => Some(list.to[C]))
      case JNothing | JNull => \/.right(None)
      case x => \/.left(InvalidFormat)
    })

  implicit def jsonFieldReader[A](implicit jValueReader: JValueReader[A]): ParamReader[ValidatedOption, (NamedParam[Field[A]], JObject), A] =
    ParamReader[ValidatedOption, (NamedParam[Field[A]], JObject), A](Function.tupled { (namedParam, jObject) =>
    jValueReader.reader(jObject \ namedParam.name).leftMap(_ =>
      NonEmptyList(ValidationError(
        s"${namedParam.param.prettyName.getOrElse(namedParam.name)} does not have the proper format",
        FieldName(namedParam.name)))).validation
  })

  implicit def jsonBodyFieldBinder[L <: HList](implicit strategy: FieldBindingStrategy[L, JObject, bindJsonFields.type]): FieldBinder.Aux[L, JObject, strategy.R] = strategy.fieldBinder

  implicit def jsonRequestBodyReader[L <: HList](implicit binder: FieldBinder[L, JObject]): ParamReader[Validated, (NamedParam[JsonBody[Fields[L]]], Request), binder.R] =
    ParamReader[Validated, (NamedParam[JsonBody[Fields[L]]], Request), binder.R](Function.tupled { (namedParam, request) =>
      val jsonBody = JsonMethods.parse(request.body)
      jsonBody match {
        case j: JObject => binder(namedParam.param.fields).run(j)
        case _ => Validation.failure(NonEmptyList(ValidationError("Request body was not a valid JSON object")))
      }
    })
}

object bindJsonFields extends Poly1 {
  implicit def atField[K, A, O](implicit npc: NamedParamConverter[K], reader: ParamReader[Validated, (NamedParam[A], JObject), O]) = at[FieldType[K, A]] { param =>
    val namedParam: NamedParam[A] = npc.asNamedParam(param)
    reader.reader.local((jObject: JObject) => (namedParam, jObject))
  }
}
