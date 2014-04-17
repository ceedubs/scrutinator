package net.ceedubs.scrutinator

import org.specs2.mock.Mockito
import shapeless._
import shapeless.syntax.singleton._
import shapeless.test.illTyped
import scalaz._
import scalaz.syntax.std.option._
import javax.servlet.http.HttpServletRequest
import scala.collection.JavaConverters._
import ParamError._

class RequestBindingSpec extends Spec with Mockito {
  import RequestBindingSpec._
  import Field._
  import ValueSource._

  "Request binding" should {
    "successfully bind valid params" ! prop { (first: Option[String], second: Option[String]) =>
      val mockRequest = mock[HttpServletRequest]
      mockRequest.getParameterMap returns Map(
        "first" -> first.map(s => Array(s)).getOrElse(Array())
      ).asJava
      mockRequest.getHeader("second") returns second.orNull
      val fields =
        ("first" ->> QueryParam(Field[String]())) ::
        ('second ->> HeaderParam(Field[String]().check(InvalidFormat, "oops!")(_ => true))) ::
        HNil

      val results = RequestBinding.fieldBinder(fields).run(mockRequest)

      typed[Errors \/ (Option[String], Option[String])](results.map(params =>
        (params.get("first"), params.get('second))))

      results must beLike {
        case \/-(params) =>
          first.flatMap(blankOption) ==== params.get("first")
          second.flatMap(blankOption) ==== params.get('second)
      }
    }

    "produce validation errors for invalid params" ! prop { (first: String, second: String) =>
      (!first.isEmpty && !second.isEmpty) ==> {
        val mockRequest = mock[HttpServletRequest]
        mockRequest.getParameterMap returns Map(
          "first" -> Array(first)
        ).asJava
        mockRequest.getHeader("second") returns second
        val fields =
          ("first" ->> QueryParam(Field[String]().check(ForcedError, "first failed!")(_ => false))) ::
          ("second" ->> HeaderParam(Field[String]().check(ForcedError, "second failed!")(_ => false))) ::
          HNil

        val results = RequestBinding.fieldBinder(fields).run(mockRequest)

        typed[Errors \/ (Option[String], Option[String])](results.map(params =>
          (params.get("first"), params.get("second"))))

        results ==== \/.left(NonEmptyList(
          ScopedValidationFail(
            ValidationFail(ForcedError, Some("first failed!")),
            FieldC("first", None) :: Nil),
          ScopedValidationFail(
            ValidationFail(ForcedError, Some("second failed!")),
            FieldC("second", None) :: Nil)))
      }
    }

    "fail to compile if an invalid param is accessed" in {
      val mockRequest = mock[HttpServletRequest]
      mockRequest.getParameterMap returns Map.empty[String, Array[String]].asJava
      val fields =
        ("first" ->> QueryParam(Field[String]())) ::
        ("second" ->> QueryParam(Field[String]())) ::
        HNil

      RequestBinding.fieldBinder(fields).run(mockRequest) must beLike {
        case \/-(params) =>
          typed[Option[String]](params.get("first")) // compiles
          typed[Option[String]](params.get("second")) // compiles
          illTyped("""params.get("not-a-param")""") // wouldn't compile
          ok // as long as this test compiles, it should pass
      }
    }

    "bind required params" ! prop { (first: Option[String]) =>
      val mockRequest = mock[HttpServletRequest]
      mockRequest.getParameterMap returns Map(
        "first" -> first.map(s => Array(s)).getOrElse(Array())
      ).asJava
      val fields =
        ('first ->> QueryParam(Field[String]().required(p => s"Hey! '${p.name}' is a required field!"))) ::
        HNil

      val expected = first.filterNot(_.isEmpty).toRightDisjunction(NonEmptyList(
        ScopedValidationFail(
          ValidationFail(Required, Some("Hey! 'first' is a required field!")),
          FieldC("first", None) :: Nil)))
      expected ==== RequestBinding.fieldBinder(fields).run(mockRequest).map(_.get('first))

    }

    "bind default params" ! prop { (first: Option[String]) =>
      val mockRequest = mock[HttpServletRequest]
      mockRequest.getParameterMap returns Map(
        "first" -> first.map(s => Array(s)).getOrElse(Array.empty)
      ).asJava
      val fields =
        ("first" ->> QueryParam(Field[String]().withDefault("<none provided>"))) ::
        HNil

      val expected  = \/.right[Errors, String](first.filterNot(_.isEmpty).getOrElse("<none provided>"))
      val result = RequestBinding.fieldBinder(fields).run(mockRequest).map(_.get("first"))
      typed[Errors \/ String](result)
      expected ==== result
    }
  }
}

object RequestBindingSpec {
  def blankOption(s: String): Option[String] = if (s.isEmpty) None else Some(s)
}
