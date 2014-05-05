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
import collection.generic.CanBuildFrom
import ValidationFail._

object JsonReaders {
  type JValueReader[A] = ParamReader[ValidatedOption, JValue, A]

  object JValueReader {
    def reader[A](f: Function2[CursorHistory, JValue, ValidatedOption[A]]): JValueReader[A] =
      ParamReader[ValidatedOption, JValue, A](f)
  }
}

trait JsonReaders {
  import JsonReaders._

  implicit val stringJValueReader: JValueReader[String] =
    JValueReader.reader( (history, jValue) => jValue match {
        case JString(s) => Validation.success(Some(s))
        case JNothing | JNull => Validation.success(None)
        case x => invalidFormat(history, "string")
    })

  implicit val intJValueReader: JValueReader[Int] =
    JValueReader.reader( (history, jValue) => jValue match {
        case JInt(x) => Validation.success(Some(x.toInt))
        case JNothing | JNull => Validation.success(None)
        case x => invalidFormat(history, "integer")
    })

  implicit val longJValueReader: JValueReader[Long] =
    JValueReader.reader( (history, jValue) => jValue match {
      case JInt(x) => Validation.success(Some(x.toLong))
      case JNothing | JNull => Validation.success(None)
      case x => invalidFormat(history, "long")
    })

  implicit val doubleJValueReader: JValueReader[Double] =
    JValueReader.reader( (history, jValue) => jValue match {
      case JDouble(x) => Validation.success(Some(x.toDouble))
      case JInt(x) => Validation.success(Some(x.toDouble))
      case JNothing | JNull => Validation.success(None)
      case x => invalidFormat(history, "double")
    })

  implicit val floatJValueReader: JValueReader[Float] =
    JValueReader.reader( (history, jValue) => jValue match {
      case JDouble(x) => Validation.success(Some(x.toFloat))
      case JDecimal(x) => Validation.success(Some(x.toFloat))
      case JInt(x) => Validation.success(Some(x.toFloat))
      case JNothing | JNull => Validation.success(None)
      case x => invalidFormat(history, "float")
    })

  implicit val shortJValueReader: JValueReader[Short] =
    JValueReader.reader( (history, jValue) => jValue match {
      case JInt(x) => Validation.success(Some(x.toShort))
      case JNothing | JNull => Validation.success(None)
      case x => invalidFormat(history, "short")
    })

  implicit val booleanJValueReader: JValueReader[Boolean] =
    JValueReader.reader( (history, jValue) => jValue match {
      case JBool(b) => Validation.success(Some(b))
      case JNothing | JNull => Validation.success(None)
      case x => invalidFormat(history, "boolean")
    })

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
        jValueReader.reader((field :: history, (j \ field.name)))
      case JNothing | JNull => Validation.success(None)
      case x => invalidFormat(history, "JSON object")
    })

  implicit def jsonBodyFieldBinder[L <: HList](implicit strategy: FieldBindingStrategy[L, JValue, bindJsonFields.type]): FieldBinder.Aux[L, JValue, strategy.R] = strategy.fieldBinder

  implicit def nestedJsonObjectReader[M, L <: HList](implicit asModel: AsModel.Aux[M, L], binder: FieldBinder[L, JValue]): ParamReader[ValidatedOption, (NamedParam[ModelField[M]], JValue), binder.R] =
    ParamReader[ValidatedOption, (NamedParam[ModelField[M]], JValue), binder.R] { case (history, (namedParam, jValue)) => {
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
    ParamReader[ValidatedOption, (NamedParam[ModelCollectionField[C, M]], JValue), C[O]] { case (history, (namedParam, jValue)) => {
      val fieldC = FieldC(namedParam.name, namedParam.param.prettyName)
      val updatedHistory = fieldC :: history
      (jValue \ namedParam.name) match {
        case JArray(l) =>
          (Traverse[List].traverseU(l.zipWithIndex) { case (jValueAtIndex, index) =>
            val nestedHistory = IndexC(index) :: history
            binder(asModel(namedParam.param.model).fields)/*.map(Option(_))*/.run((nestedHistory, jValueAtIndex))
            //entryReader.reader((nestedHistory, jValue)).flatMap(o => std.option.toSuccess(o)(
            //  invalidFormatNel(nestedHistory, "JSON array with no null or undefined elements")))
          }).map(list => Option(list.to[C]))

        // TODO
        //case j: JObject => binder(asModel(namedParam.param.model).fields).map(Some(_)).run((
        //  updatedHistory, j))
        case JNothing | JNull => Validation.success(None)
        case x => invalidFormat(updatedHistory, "JSON object")
      }
    }
  }

  implicit def jsonRequestBodyReader[M, L <: HList](implicit asModel: AsModel.Aux[M, L], binder: FieldBinder[L, JValue]): ParamReader[Validated, (NamedParam[JsonParam[ModelField[M]]], Request), binder.R] =
    ParamReader[Validated, (NamedParam[JsonParam[ModelField[M]]], Request), binder.R] { case (history, (namedParam, request)) =>
      Validation.fromTryCatch(JsonMethods.parse(request.body)).
      leftMap(_ => invalidFormatNel(history, "JSON body")).
      flatMap(jsonBody => binder(asModel(namedParam.param.model).fields).run((history, jsonBody)))
    }
}

object bindJsonFields extends Poly1 {
  implicit def atField[K, A, O](implicit npc: NamedParamConverter[K], reader: ParamReader[Validated, (NamedParam[A], JValue), O]) = at[FieldType[K, A]] { param =>
    val namedParam: NamedParam[A] = npc.asNamedParam(param)
    reader.local((j: JValue) => (namedParam, j)).reader
  }
}
