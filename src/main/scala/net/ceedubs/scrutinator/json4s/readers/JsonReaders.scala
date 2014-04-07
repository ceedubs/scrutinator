package net.ceedubs.scrutinator
package json4s
package readers

import net.ceedubs.scrutinator.readers._
import scalaz._
import shapeless._
import shapeless.record._
import org.json4s._
import org.json4s.jackson.JsonMethods
import org.scalatra.validation.{ FieldName, ValidationError }

case class JsonObjectParam[L <: HList](fields: L)

object JsonParam {
  import Param._

  type JsonFieldParam[A] = Param[A, ValueSource.Json]

  def jsonFieldParam[A](
      description: Option[String] = Defaults.description,
      notes: Option[String] = Defaults.notes,
      prettyName: Option[String] = Defaults.prettyName,
      validations: ParamValidations[A] = Defaults.validations[A]): JsonFieldParam[A] = {

    Param[A, ValueSource.Json](description, notes, prettyName, validations)
  }
}


trait JsonReaders {

  implicit def jsonBodyFieldBinder[L <: HList](implicit strategy: FieldBindingStrategy[L, JObject, bindJsonFields.type]): FieldBinder.Aux[L, JObject, strategy.R] = strategy.fieldBinder

  implicit def jsonRequestBodyReader[L <: HList](implicit binder: FieldBinder[L, JObject]): ParamReader[Validated, (NamedParam[JsonObjectParam[L]], Request), binder.R] =
    ParamReader[Validated, (NamedParam[JsonObjectParam[L]], Request), binder.R](Function.tupled { (namedParam, request) =>
      val jsonBody = JsonMethods.parse(request.body)
      jsonBody match {
        case j: JObject => binder(namedParam.param.fields).run(j)
        case _ => Validation.failure(NonEmptyList(ValidationError("Request body was not a valid JSON object")))
      }
    })

  implicit def stringJsonFieldReader: ParamReader[ValidatedOption, (NamedParam[JsonParam.JsonFieldParam[String]], JObject), String] =
    ParamReader[ValidatedOption, (NamedParam[JsonParam.JsonFieldParam[String]], JObject), String](Function.tupled { (namedParam, jObject) =>
    (jObject \ namedParam.name) match {
      case JString(s) => Validation.success(Some(s))
      case JNothing | JNull => Validation.success(None)
      case x => Validation.failure(NonEmptyList(ValidationError(s"${namedParam.param.prettyName.getOrElse(namedParam.name)} must be a valid string", FieldName(namedParam.name))))
    }
  })

  implicit def intJsonFieldReader: ParamReader[ValidatedOption, (NamedParam[JsonParam.JsonFieldParam[Int]], JObject), Int] =
    ParamReader[ValidatedOption, (NamedParam[JsonParam.JsonFieldParam[Int]], JObject), Int](Function.tupled { (namedParam, jObject) =>
    (jObject \ namedParam.name) match {
      case JInt(i) => Validation.success(Some(i.toInt))
      case JNothing | JNull => Validation.success(None)
      case x => Validation.failure(NonEmptyList(ValidationError(s"${namedParam.param.prettyName.getOrElse(namedParam.name)} must be a valid integer", FieldName(namedParam.name))))
    }
  })

  implicit def longFieldReader: ParamReader[ValidatedOption, (NamedParam[JsonParam.JsonFieldParam[Long]], JObject), Long] =
    ParamReader[ValidatedOption, (NamedParam[JsonParam.JsonFieldParam[Long]], JObject), Long](Function.tupled { (namedParam, jObject) =>
    (jObject \ namedParam.name) match {
      case JInt(i) => Validation.success(Some(i.toLong))
      case JNothing | JNull => Validation.success(None)
      case x => Validation.failure(NonEmptyList(ValidationError(s"${namedParam.param.prettyName.getOrElse(namedParam.name)} must be a valid long", FieldName(namedParam.name))))
    }
  })

  implicit def doubleFieldReader: ParamReader[ValidatedOption, (NamedParam[JsonParam.JsonFieldParam[Double]], JObject), Double] =
    ParamReader[ValidatedOption, (NamedParam[JsonParam.JsonFieldParam[Double]], JObject), Double](Function.tupled { (namedParam, jObject) =>
    (jObject \ namedParam.name) match {
      case JDouble(i) => Validation.success(Some(i.toDouble))
      case JInt(i) => Validation.success(Some(i.toDouble))
      case JNothing | JNull => Validation.success(None)
      case x => Validation.failure(NonEmptyList(ValidationError(s"${namedParam.param.prettyName.getOrElse(namedParam.name)} must be a valid double", FieldName(namedParam.name))))
    }
  })

  implicit def floatFieldReader: ParamReader[ValidatedOption, (NamedParam[JsonParam.JsonFieldParam[Float]], JObject), Float] =
    ParamReader[ValidatedOption, (NamedParam[JsonParam.JsonFieldParam[Float]], JObject), Float](Function.tupled { (namedParam, jObject) =>
    (jObject \ namedParam.name) match {
      case JDouble(i) => Validation.success(Some(i.toFloat))
      case JDecimal(i) => Validation.success(Some(i.toFloat))
      case JNothing | JNull => Validation.success(None)
      case x => Validation.failure(NonEmptyList(ValidationError(s"${namedParam.param.prettyName.getOrElse(namedParam.name)} must be a valid float", FieldName(namedParam.name))))
    }
  })

  implicit def shortFieldReader: ParamReader[ValidatedOption, (NamedParam[JsonParam.JsonFieldParam[Short]], JObject), Short] =
    ParamReader[ValidatedOption, (NamedParam[JsonParam.JsonFieldParam[Short]], JObject), Short](Function.tupled { (namedParam, jObject) =>
    (jObject \ namedParam.name) match {
      case JInt(i) => Validation.success(Some(i.toShort))
      case JNothing | JNull => Validation.success(None)
      case x => Validation.failure(NonEmptyList(ValidationError(s"${namedParam.param.prettyName.getOrElse(namedParam.name)} must be a valid short", FieldName(namedParam.name))))
    }
  })

  implicit def booleanFieldReader: ParamReader[ValidatedOption, (NamedParam[JsonParam.JsonFieldParam[Boolean]], JObject), Boolean] =
    ParamReader[ValidatedOption, (NamedParam[JsonParam.JsonFieldParam[Boolean]], JObject), Boolean](Function.tupled { (namedParam, jObject) =>
    (jObject \ namedParam.name) match {
      case JBool(b) => Validation.success(Some(b))
      case JNothing | JNull => Validation.success(None)
      case x => Validation.failure(NonEmptyList(ValidationError(s"${namedParam.param.prettyName.getOrElse(namedParam.name)} must be a valid boolean", FieldName(namedParam.name))))
    }
  })
}

object bindJsonFields extends Poly1 {
  implicit def atField[K, A, O](implicit npc: NamedParamConverter[K], reader: ParamReader[Validated, (NamedParam[A], JObject), O]) = at[FieldType[K, A]] { param =>
    val namedParam: NamedParam[A] = npc.asNamedParam(param)
    reader.reader.local((jObject: JObject) => (namedParam, jObject))
  }
}
