package net.ceedubs.scrutinator
package json4s
package readers

import net.ceedubs.scrutinator.readers._
import scalaz._
import scalaz.std.list._
import scalaz.std.option._
import shapeless._
import shapeless.record._
import org.json4s._
import org.json4s.jackson.JsonMethods
import collection.generic.CanBuildFrom
import ValidationFail._

object JsonReaders {
  type JValueReader[A] = ParamReader[ValidatedOption, JValue, A]

  object JValueReader {
    def reader[A](f: Function2[CursorHistory, JValue, ValidatedOption[A]]): JValueReader[A] =
      ParamReader.paramReader[ValidatedOption, JValue, A](f)

    def simpleReader[A](expectedType: String)(f: PartialFunction[JValue, A]): JValueReader[A] =
      ParamReader.paramReader[ValidatedOption, JValue, A]((history, jValue) =>
        f.
        andThen((a: A) => Validation.success(Some(a))).
        orElse[JValue, ValidatedOption[A]]{
          case JNothing | JNull => Validation.success(None)
          case _ => invalidFormat(history, expectedType)
        }.
        apply(jValue))
  }
}

trait JsonReaders {
  import JsonReaders._

  implicit val stringJValueReader: JValueReader[String] = JValueReader.simpleReader("string"){
    case JString(s) => s
  }

  implicit val intJValueReader: JValueReader[Int] = JValueReader.simpleReader("integer"){
    case JInt(x) if x.isValidInt => x.toInt
  }

  implicit val longJValueReader: JValueReader[Long] = JValueReader.simpleReader("long"){
    case JInt(x) if x.isValidLong => x.toLong
  }

  implicit val doubleJValueReader: JValueReader[Double] = JValueReader.simpleReader("double"){
    case JDouble(x) => x.toDouble
    case JInt(x) if x.isValidDouble => x.toDouble
  }

  implicit val floatJValueReader: JValueReader[Float] = JValueReader.simpleReader("float"){
    case JDouble(x) if x >= Float.MinValue && x <= Float.MaxValue => x.toFloat
    case JDecimal(x) if x.isValidFloat => x.toFloat
    case JInt(x) if x.isValidFloat => x.toFloat
  }

  implicit val shortJValueReader: JValueReader[Short] = JValueReader.simpleReader("short"){
    case JInt(x) if x.isValidShort => x.toShort
  }

  implicit val booleanJValueReader: JValueReader[Boolean] = JValueReader.simpleReader("boolean"){
    case JBool(b) => b
  }

  implicit def cbfJValueReader[C[_], A](implicit entryReader: JValueReader[A], cbf: CanBuildFrom[Nothing, A, C[A]]): JValueReader[C[A]] =
    JValueReader.reader( (history, jValue) => jValue match {
      case JArray(l) =>
        (Traverse[List].traverseU(l.zipWithIndex) { case (jValue, index) =>
          val nestedHistory = IndexC(index) :: history
          entryReader.reader((nestedHistory, jValue)).flatMap(o => std.option.toSuccess(o)(
            invalidFormatNel(nestedHistory, "JSON array with no null or undefined elements")))
        }).map(list => Some(list.to[C]))
      case JNothing | JNull => Validation.success(None)
      case x => invalidFormat(history, "JSON array")
    })

  implicit def jsonFieldReader[A](implicit jValueReader: JValueReader[A]): FieldReader[ValidatedOption, JValue, A] =
    FieldReader.reader( (history, field, jValue) => jValue match {
      case j: JObject =>
        jValueReader.reader((history, (j \ field.name)))
      case JNothing | JNull => Validation.success(None)
      case x => invalidFormat(history, "JSON object")
    })

  implicit def jsonBodyFieldBinder[L <: HList](implicit strategy: FieldBindingStrategy[L, JValue, bindJsonFields.type]): FieldBinder.Aux[L, JValue, strategy.R] = strategy.fieldBinder

  implicit def nestedJsonObjectReader[M, L <: HList](implicit asModel: AsModel.Aux[M, L], binder: FieldBinder[L, JValue]): ParamReader[ValidatedOption, (NamedParam[ModelField[M]], JValue), binder.R] =
    ParamReader.paramReader[ValidatedOption, (NamedParam[ModelField[M]], JValue), binder.R] { case (history, (namedParam, jValue)) => {
      val fieldC = FieldC(namedParam.name, namedParam.param.prettyName)
      val updatedHistory = fieldC :: history
      (jValue \ namedParam.name) match {
        case j: JObject => binder(asModel(namedParam.param.model).fields).map(Some(_)).run((
          updatedHistory, j))
        case JNothing | JNull => Validation.success(None)
        case x => invalidFormat(updatedHistory, "JSON object")
      }
    }
  }

  implicit def nestedJsonCollectionReader[C[_], M, L <: HList, O <: HList](implicit asModel: AsModel.Aux[M, L], binder: FieldBinder.Aux[L, JValue, O], cbf: CanBuildFrom[Nothing, O, C[O]]): ParamReader[ValidatedOption, (NamedParam[ModelCollectionField[C, M]], JValue), C[O]] =
    ParamReader.paramReader[ValidatedOption, (NamedParam[ModelCollectionField[C, M]], JValue), C[O]] { case (history, (namedParam, jValue)) => {
      val fieldC = FieldC(namedParam.name, namedParam.param.prettyName)
      val updatedHistory = fieldC :: history
      (jValue \ namedParam.name) match {
        case JArray(l) =>
          (Traverse[List].traverseU(l.zipWithIndex) { case (jValueAtIndex, index) =>
            val nestedHistory = IndexC(index) :: history
            binder(asModel(namedParam.param.model).fields).run((nestedHistory, jValueAtIndex))
          }).map(list => Option(list.to[C]))
        case JNothing | JNull => Validation.success(None)
        case x => invalidFormat(updatedHistory, "JSON array")
      }
    }
  }

  implicit def jsonRequestBodyReader[M, L <: HList](implicit asModel: AsModel.Aux[M, L], binder: FieldBinder[L, JValue]): ParamReader[ValidatedOption, (NamedParam[JsonParam[ModelField[M]]], Request), binder.R] =
    ParamReader.paramReader[ValidatedOption, (NamedParam[JsonParam[ModelField[M]]], Request), binder.R] { case (history, (namedParam, request)) =>
      Validation.fromTryCatch(JsonMethods.parse(request.body)).
      leftMap(_ => invalidFormatNel(history, "JSON body")).
      map{
        case JNothing | JNull => None
        case x => Some(x)
      }.
      flatMap(o => Traverse[Option].traverse(o)(jsonBody => binder(asModel(namedParam.param.model).fields).run((history, jsonBody))))
    }
}

object bindJsonFields extends Poly1 {
  implicit def atField[K, A, O](implicit npc: NamedParamConverter[K], reader: ParamReader[Validated, (NamedParam[A], JValue), O]) = at[FieldType[K, A]] { param =>
    val namedParam: NamedParam[A] = npc.asNamedParam(param)
    reader.local((j: JValue) => (namedParam, j)).reader
  }
}
