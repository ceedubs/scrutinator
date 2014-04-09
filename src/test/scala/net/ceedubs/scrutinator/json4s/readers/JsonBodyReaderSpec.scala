package net.ceedubs.scrutinator
package json4s
package readers

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import shapeless._
import shapeless.syntax.singleton._
import scalaz._
import scalaz.syntax.std.option._
import scala.collection.JavaConverters._
import org.scalatra.validation.{ FieldName, ValidationError }

class JsonBodyReaderSpec extends Spec {
  import Param._
  import ValueSource._

 "A Json body reader" should {
    "successfully bind valid params" ! prop { (string: Option[String], int: Option[Int], long: Option[Long], double: Option[Double], float: Option[Float], short: Option[Short], boolean: Option[Boolean]) =>
      val fields =
        ("body" ->> JsonBody(Fields(
          ("string" ->> Param[String]()) ::
          ("int" ->> Param[Int]()) ::
          ("long" ->> Param[Long]()) ::
          ("double" ->> Param[Double]()) ::
          ("float" ->> Param[Float]()) ::
          ("short" ->> Param[Short]()) ::
          ("boolean" ->> Param[Boolean]()) ::
          HNil))
        ) :: HNil

      val body =
        ("string" -> string) ~
        ("int" -> int) ~
        ("long" -> long) ~
        ("double" -> double) ~
        ("float" -> float) ~
        ("short" -> short.map(_.toInt)) ~
        ("boolean" -> boolean)
      val request = mockRequest(jsonBody = Some(compact(render(body))))

      val results = RequestBinding.bindFromRequest(fields).run(request).map { params =>
        val body = params.get("body")
        (body.get("string"), body.get("int"), body.get("long"), body.get("double"), body.get("float"), body.get("short"), body.get("boolean"))
      }
      \/.right[Errors, (Option[String], Option[Int], Option[Long], Option[Double], Option[Float], Option[Short], Option[Boolean])]((string, int, long, double, float, short, boolean)) ==== results
    }

    "return validation errors for invalid params" ! prop { (int: Int, boolean: Boolean) =>
      val fields =
        ("body" ->> JsonBody(Fields(
          ("string1" ->> Param[String](prettyName = Some("String 1"))) ::
          ("string2" ->> Param[String]()) ::
          HNil))
        ) :: HNil

      val body = ("string1" -> int) ~ ("string2" -> boolean)
      val request = mockRequest(jsonBody = Some(compact(render(body))))

      RequestBinding.bindFromRequest(fields).run(request) must beLike {
        case -\/(errors) => errors ==== NonEmptyList(
          ValidationError("String 1 must be a valid string", FieldName("string1")),
          ValidationError("string2 must be a valid string", FieldName("string2")))
      }
    }

  }
}
