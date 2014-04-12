package net.ceedubs.scrutinator
package readers

import scalaz.{ @@ => _, _ }
import shapeless.tag._
import shapeless.tag
import org.scalatra.validation.{ FieldName, ValidationError }

trait HeaderReaders {
  import HeaderReaders._
  import Field._

  implicit def headerNamedParamReader[A](implicit reader: FieldReader[ValidatedOption, HeaderParams, A]): ParamReader[ValidatedOption, (NamedParam[HeaderParam[Field[A]]], Request), A] = {
    ParamReader[ValidatedOption, (NamedParam[HeaderParam[Field[A]]], Request), A] { case (history, (namedParam, request)) =>
      val fieldC = FieldC(name = namedParam.name, prettyName = namedParam.param.prettyName)
      val headers = HeaderParams(request.headers)
      val nestedHistory = fieldC :: history
      reader.reader((history, (fieldC, headers))).flatMap { maybeA =>
        std.option.cata(maybeA)({ a =>
          val errors = namedParam.param.validations.
            map(_.apply(fieldC, a)
            .map(ScopedValidationFail(_, nestedHistory))).
            flatten
          std.option.toFailure(std.list.toNel(errors))(Option(a))
        }, Validation.success(None))
      }
    }
  }

  implicit val headerStringFieldReader: FieldReader[ValidatedOption, HeaderParams, String] =
    ParamReader { case (history, (fieldC, headerParams)) =>
        Validation.success(headerParams.get(fieldC.name).filterNot(_.isEmpty))
    }

  implicit val intHeaderReader: FieldReader[ValidatedOption, HeaderParams, Int] =
    FieldReader.parseField("integer")(std.string.parseInt)

  implicit val longHeaderReader: FieldReader[ValidatedOption, HeaderParams, Long] =
    FieldReader.parseField("long")(std.string.parseLong)

  implicit val byteHeaderReader: FieldReader[ValidatedOption, HeaderParams, Byte] =
    FieldReader.parseField("byte")(std.string.parseByte)

  implicit val doubleHeaderReader: FieldReader[ValidatedOption, HeaderParams, Double] =
    FieldReader.parseField("double")(std.string.parseDouble)

  implicit val floatHeaderReader: FieldReader[ValidatedOption, HeaderParams, Float] =
    FieldReader.parseField("float")(std.string.parseFloat)

  implicit val shortHeaderReader: FieldReader[ValidatedOption, HeaderParams, Short] =
    FieldReader.parseField("short")(std.string.parseShort)

}

object HeaderReaders extends HeaderReaders {
  import ValueSource.Headers

  type HeaderParams = Map[String, String] @@ Headers

  object HeaderParams {
    val tagger: Tagger[Headers] = tag[Headers]
    def apply(params: Map[String, String]): HeaderParams = tagger[Map[String, String]](params)
  }
}
