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
import ParamError._

class NumberReadersSpec extends Spec with Mockito {
  import Field._
  import ValueSource._

  def asParam(key: String, value: Option[Any]) =
    key -> value.map(a => Array(a.toString)).getOrElse(Array())

 "Number param readers" should {
    "successfully bind valid numbers in the query string" ! prop { (int: Option[Int], long: Option[Long],
        byte: Option[Byte], double: Option[Double], float: Option[Float], short: Option[Short], doubles: Array[Double]) =>

      val mockRequest = mock[HttpServletRequest]
      mockRequest.getParameterMap returns Map(
        asParam("int", int),
        asParam("long", long),
        asParam("byte", byte),
        asParam("double", double),
        asParam("float", float),
        asParam("short", short),
        "doubles" -> doubles.map(_.toString)
      ).asJava
      val fields =
        ("int" ->> QueryParam(Field[Int]())) ::
        ("long" ->> QueryParam(Field[Long]())) ::
        ("byte" ->> QueryParam(Field[Byte]())) ::
        ("double" ->> QueryParam(Field[Double]())) ::
        ("float" ->> QueryParam(Field[Float]())) ::
        ("short" ->> QueryParam(Field[Short]())) ::
        ("doubles" ->> QueryParam(Field[Set[Double]]())) ::
        HNil

      RequestBinding.fieldBinder(fields).run(mockRequest) must beLike {
        case \/-(params) =>
          int ==== params.get("int")
          long ==== params.get("long")
          byte ==== params.get("byte")
          double ==== params.get("double")
          float ==== params.get("float")
          short ==== params.get("short")
          Option(doubles.toSet) ==== params.get("doubles")
      }
    }

    "successfully bind valid numbers in headers" ! prop { (int: Option[Int], long: Option[Long],
        byte: Option[Byte], double: Option[Double], float: Option[Float], short: Option[Short]) =>

      val mockRequest = mock[HttpServletRequest]
      mockRequest.getHeader("int") returns int.map(_.toString).orNull
      mockRequest.getHeader("long") returns long.map(_.toString).orNull
      mockRequest.getHeader("byte") returns byte.map(_.toString).orNull
      mockRequest.getHeader("double") returns double.map(_.toString).orNull
      mockRequest.getHeader("float") returns float.map(_.toString).orNull
      mockRequest.getHeader("short") returns short.map(_.toString).orNull
      val fields =
        ("int" ->> HeaderParam(Field[Int]())) ::
        ("long" ->> HeaderParam(Field[Long]())) ::
        ("byte" ->> HeaderParam(Field[Byte]())) ::
        ("double" ->> HeaderParam(Field[Double]())) ::
        ("float" ->> HeaderParam(Field[Float]())) ::
        ("short" ->> HeaderParam(Field[Short]())) ::
        HNil

      RequestBinding.fieldBinder(fields).run(mockRequest) must beLike {
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

        val result = RequestBinding.fieldBinder(fields).run(mockRequest)

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
              ScopedValidationFail(
                ValidationFail(InvalidFormat, Some("my int must be a valid integer")),
                FieldC("int", Some("my int")) :: Nil),
              ScopedValidationFail(
                ValidationFail(InvalidFormat, Some("long must be a valid long")),
                FieldC("long", None) :: Nil),
              ScopedValidationFail(
                ValidationFail(InvalidFormat, Some("my byte must be a valid byte")),
                FieldC("byte", Some("my byte")) :: Nil),
              ScopedValidationFail(
                ValidationFail(InvalidFormat, Some("double must be a valid double")),
                FieldC("double", None) :: Nil),
              ScopedValidationFail(
                ValidationFail(InvalidFormat, Some("float must be a valid float")),
                FieldC("float", None) :: Nil),
              ScopedValidationFail(
                ValidationFail(InvalidFormat, Some("short must be a valid short")),
                FieldC("short", None) :: Nil))
        }
      }
    }
  }
}
