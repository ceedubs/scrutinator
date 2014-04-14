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
import ParamError._

class JsonBodyReaderSpec extends Spec {
  import Field._
  import ValueSource._

 "A Json body reader" should {
    "successfully bind valid params" ! prop { (
        string: Option[String], stringField: Field[String],
        boolean: Option[Boolean], booleanField: Field[Boolean],
        stringWithDefault: Option[String], stringWithDefaultField: FieldWithDefault[String],
        requiredBoolean: Boolean, requiredBooleanField: RequiredParam[Field[Boolean]]) =>

      val fields =
        ("body" ->> JsonParam(ModelField(Model(
          ("string" ->> stringField) ::
          ("boolean" ->> booleanField) ::
          ("stringWithDefault" ->> stringWithDefaultField) ::
          ("requiredBoolean" ->> requiredBooleanField) :: HNil)))
        ) :: HNil

      val body =
        ("string" -> string) ~
        ("boolean" -> boolean) ~
        ("stringWithDefault" -> stringWithDefault) ~
        ("requiredBoolean" -> requiredBoolean)
      val request = mockRequest(jsonBody = Some(compact(render(body))))

      val results = RequestBinding.fieldBinder(fields).run(request).map { params =>
        val body = params.get("body")
        (body.get("string"), body.get("boolean"), body.get("stringWithDefault"), body.get("requiredBoolean"))
      }
      \/.right[Errors, (Option[String], Option[Boolean], String, Boolean)]((
        string,
        boolean,
        stringWithDefault.getOrElse(stringWithDefaultField.default),
        requiredBoolean)) ==== results
    }

    "successfully bind numbers" ! prop { (
        int: Option[Int], intField: Field[Int],
        long: Option[Long],
        double: Option[Double],
        float: Option[Float],
        short: Option[Short]) =>

      val fields =
        ("body" ->> JsonParam(ModelField(Model(
          ("int" ->> intField) ::
          ("long" ->> Field[Long]()) ::
          ("double" ->> Field[Double]()) ::
          ("float" ->> Field[Float]()) ::
          ("short" ->> Field[Short]()) :: HNil)))
        ) :: HNil

      val body =
        ("int" -> int) ~
        ("long" -> long) ~
        ("double" -> double) ~
        ("float" -> float) ~
        ("short" -> short.map(_.toInt))
      val request = mockRequest(jsonBody = Some(compact(render(body))))

      val results = RequestBinding.fieldBinder(fields).run(request).map { params =>
        val body = params.get("body")
        (body.get("int"), body.get("long"), body.get("double"), body.get("float"), body.get("short"))
      }
      \/.right[Errors, (Option[Int], Option[Long], Option[Double], Option[Float], Option[Short])]((int, long, double, float, short)) ==== results
    }

    "successfully bind collections" ! prop { (stringSet: Option[Set[String]], doubleList: Option[List[Double]]) =>
      val fields =
        ("body" ->> JsonParam(ModelField(Model(
          ("stringSet" ->> Field[Set[String]]()) ::
          ("doubleList" ->> Field[List[Double]]()) :: HNil)))
        ) :: HNil
      val body =
        ("stringSet" -> stringSet) ~
        ("doubleList" -> doubleList)
      val request = mockRequest(jsonBody = Some(compact(render(body))))

      val results = RequestBinding.fieldBinder(fields).run(request).map { params =>
        val body = params.get("body")
        (body.get("stringSet"), body.get("doubleList"))
      }
      \/.right[Errors, (Option[Set[String]], Option[List[Double]])]((stringSet, doubleList)) ==== results
    }

    "return validation errors for invalid params" ! prop { (
        stringField: Field[String], int: Int,
        doubleField: Field[Double], string: String,
        intSetField: Field[Set[Int]],
        booleanListField: Field[List[Boolean]], intNel: NonEmptyList[Int]) =>

      val fields =
        ("body" ->> JsonParam(ModelField(Model(
          ("string" ->> stringField) ::
          ("double" ->> doubleField) ::
          ("intSet" ->> intSetField) ::
          ("booleanList" ->> booleanListField) ::
          HNil)))
        ) :: HNil

      val body =
        ("string" -> int) ~
        ("double" -> string) ~
        ("intSet" -> int) ~
        ("booleanList" -> intNel.list)
      val request = mockRequest(jsonBody = Some(compact(render(body))))

      val expectedBooleanListErrors = (Traverse[NonEmptyList].mapAccumL(intNel, 0) { (index, x) =>
        val error = ScopedValidationFail(
          ValidationFail(InvalidFormat, Some(s"each element of ${booleanListField.prettyName.getOrElse("booleanList")} must be a valid boolean")),
          IndexC(index) :: FieldC("booleanList", booleanListField.prettyName) :: Nil)
        (index + 1, error)
      })._2

      val expectedOtherErrors = NonEmptyList(
          ScopedValidationFail(
            ValidationFail(InvalidFormat, Some(s"${stringField.prettyName.getOrElse("string")} must be a valid string")),
            FieldC("string", stringField.prettyName) :: Nil),
          ScopedValidationFail(
            ValidationFail(InvalidFormat, Some(s"${doubleField.prettyName.getOrElse("double")} must be a valid double")),
            FieldC("double", doubleField.prettyName) :: Nil),
          ScopedValidationFail(
            ValidationFail(InvalidFormat, Some(s"${intSetField.prettyName.getOrElse("intSet")} must be a valid JSON array")),
            FieldC("intSet", intSetField.prettyName) :: Nil))

      RequestBinding.fieldBinder(fields).run(request) must beLike {
        case -\/(errors) => errors ==== (expectedOtherErrors.append(expectedBooleanListErrors))

      }
    }

    "successfully bind nested params" ! prop { (
        string: Option[String], stringField: Field[String],
        boolean: Option[Boolean], booleanField: Field[Boolean],
        stringWithDefault: Option[String], stringWithDefaultField: FieldWithDefault[String],
        requiredBoolean: Boolean, requiredBooleanField: RequiredParam[Field[Boolean]]) =>

      val fields =
        ("body" ->> JsonParam(ModelField(Model(
          ("string" ->> stringField) ::
          ("foo" ->> ModelField(Model(
            ("boolean" ->> booleanField) ::
            ("stringWithDefault" ->> stringWithDefaultField) :: HNil))) ::
          ("requiredBoolean" ->> requiredBooleanField) :: HNil)))
        ) :: HNil

      val body =
        ("string" -> string) ~
        ("foo" -> (
          ("boolean" -> boolean) ~
          ("stringWithDefault" -> stringWithDefault))) ~
        ("requiredBoolean" -> requiredBoolean)
      val request = mockRequest(jsonBody = Some(compact(render(body))))

      val results = RequestBinding.fieldBinder(fields).run(request).map { params =>
        val body = params.get("body")
        val foo = body.get("foo")
        (body.get("string"), foo.flatMap(_.get("boolean")), foo.map(_.get("stringWithDefault")), body.get("requiredBoolean"))
      }
      \/.right[Errors, (Option[String], Option[Boolean], Option[String], Boolean)]((
        string,
        boolean,
        Some(stringWithDefault.getOrElse(stringWithDefaultField.default)),
        requiredBoolean)) ==== results
    }
  }
}
