package net.ceedubs.scrutinator
package readers

import org.specs2.mock.Mockito
import shapeless._
import shapeless.syntax.singleton._
import shapeless.test.illTyped
import scalaz._
import scalaz.syntax.std.option._
import javax.servlet.http.HttpServletRequest
import scala.collection.JavaConverters._
import org.scalatra.validation.{ FieldName, ValidationError }

class NumberReadersSpec extends Spec with Mockito {
  import Field._
  import ValueSource._

  def asParam(key: String, value: Option[Any]) =
    key -> value.map(a => Array(a.toString)).getOrElse(Array())

 "Integer param readers" should {
    "successfully bind valid numbers" ! prop { (int: Option[Int], long: Option[Long],
        byte: Option[Byte], double: Option[Double], float: Option[Float], short: Option[Short]) =>

      val mockRequest = mock[HttpServletRequest]
      mockRequest.getParameterMap returns Map(
        asParam("int", int),
        asParam("long", long),
        asParam("byte", byte),
        asParam("double", double),
        asParam("float", float),
        asParam("short", short)
      ).asJava
      val fields =
        ("int" ->> QueryParam(Field[Int]())) ::
        ("long" ->> QueryParam(Field[Long]())) ::
        ("byte" ->> QueryParam(Field[Byte]())) ::
        ("double" ->> QueryParam(Field[Double]())) ::
        ("float" ->> QueryParam(Field[Float]())) ::
        ("short" ->> QueryParam(Field[Short]())) ::
        HNil
      RequestBinding.bindFromRequest(fields).run(mockRequest) must beLike {
        case \/-(params) =>
          int ==== params.get("int")
          long ==== params.get("long")
          byte ==== params.get("byte")
          double ==== params.get("double")
          float ==== params.get("float")
          short ==== params.get("short")
      }
    }

    "provide an error message for invalid numbers" ! prop { (s: String) =>
      (s.nonEmpty && std.string.parseDouble(s).isFailure) ==> {
        val mockRequest = mock[HttpServletRequest]
        mockRequest.getParameterMap returns Map(
          asParam("int", Some(s)),
          asParam("long", Some(s)),
          asParam("byte", Some(s)),
          asParam("double", Some(s)),
          asParam("float", Some(s)),
          asParam("short", Some(s))
        ).asJava
        val fields =
          ("int" ->> QueryParam(Field[Int](prettyName = Some("my int")))) ::
          ("long" ->> QueryParam(Field[Long]())) ::
          ("byte" ->> QueryParam(Field[Byte](prettyName = Some("my byte")))) ::
          ("double" ->> QueryParam(Field[Double]())) ::
          ("float" ->> QueryParam(Field[Float]())) ::
          ("short" ->> QueryParam(Field[Short]())) ::
          HNil

        val result = RequestBinding.bindFromRequest(fields).run(mockRequest)

        val tupledResult = result.map(params => (
          params.get("int"),
          params.get("long"),
          params.get("byte"),
          params.get("double"),
          params.get("float"),
          params.get("short")))

         typed[Errors \/ (Option[Int], Option[Long], Option[Byte], Option[Double], Option[Float], Option[Short])](tupledResult)

        result must beLike {
          case -\/(errors) =>
            errors ==== NonEmptyList(
              ValidationError("my int must be a valid integer", FieldName("int")),
              ValidationError("long must be a valid long", FieldName("long")),
              ValidationError("my byte must be a valid byte", FieldName("byte")),
              ValidationError("double must be a valid double", FieldName("double")),
              ValidationError("float must be a valid float", FieldName("float")),
              ValidationError("short must be a valid short", FieldName("short")))
        }
      }
    }
  }
}
