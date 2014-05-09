package net.ceedubs.scrutinator
package scalatra
package readers

import net.ceedubs.scrutinator.readers._
import scalaz.{ @@ => _, _ }
import shapeless.tag
import shapeless.tag._
import org.scalatra.MultiParams

trait PathReaders {
  import PathReaders._
  import Field._

  implicit def pathNamedParamReader[A](implicit reader: FieldReader[ValidatedOption, PathParams, A]): ParamReader[ValidatedOption, (NamedParam[PathParam[Field[A]]], Request), A] = {
    ParamReader.paramReader[ValidatedOption, (NamedParam[PathParam[Field[A]]], Request), A] { case (history, (namedParam, request)) =>
      val multiParams: Option[Map[String, Seq[String]]] = request.get(org.scalatra.MultiParamsKey).flatMap(x =>
        if (x.isInstanceOf[MultiParams]) Some(x.asInstanceOf[MultiParams]) else None)
      val params = new org.scalatra.util.MultiMapHeadView[String, String] {
        val multiMap = multiParams.getOrElse(Map.empty)
      }
      val readerWithValidations = ParamReader.andThenCheckField(reader)((nestedHistory, fieldC, a) =>
        Field.runValidations(namedParam.param, fieldC, nestedHistory, a))
      val fieldC = FieldC(name = namedParam.name, prettyName = namedParam.param.prettyName) 
      val nestedHistory = fieldC :: history
      readerWithValidations.reader((nestedHistory, (fieldC, PathParams(params))))
    }
  }

  implicit val pathStringFieldReader: FieldReader[ValidatedOption, PathParams, String] =
    FieldReader.reader[ValidatedOption, PathParams, String]((history, fieldC, pathParams) =>
      Validation.success(pathParams.get(fieldC.name).filterNot(_.isEmpty)))

  implicit val intPathReader: FieldReader[ValidatedOption, PathParams, Int] =
    FieldReader.parseField("integer")(std.string.parseInt)

  implicit val longPathReader: FieldReader[ValidatedOption, PathParams, Long] =
    FieldReader.parseField("long")(std.string.parseLong)

  implicit val bytePathReader: FieldReader[ValidatedOption, PathParams, Byte] =
    FieldReader.parseField("byte")(std.string.parseByte)

  implicit val doublePathReader: FieldReader[ValidatedOption, PathParams, Double] =
    FieldReader.parseField("double")(std.string.parseDouble)

  implicit val floatPathReader: FieldReader[ValidatedOption, PathParams, Float] =
    FieldReader.parseField("float")(std.string.parseFloat)

  implicit val shortPathReader: FieldReader[ValidatedOption, PathParams, Short] =
    FieldReader.parseField("short")(std.string.parseShort)
}

object PathReaders extends PathReaders {
  import ValueSource.Path

  type PathParams = Map[String, String] @@ Path

  object PathParams {
    val tagger: Tagger[Path] = tag[Path]
    def apply(params: Map[String, String]): PathParams = tagger[Map[String, String]](params)
  }
}
