package net.ceedubs.scrutinator

import org.specs2.mock.Mockito
import shapeless._
import shapeless.syntax.singleton._
import shapeless.test.illTyped
import scalaz._
import scalaz.syntax.std.option._
import javax.servlet.http.HttpServletRequest
import scala.collection.JavaConverters._
import org.scalatra.validation.{ FieldName, ValidationError }

class RequestBindingSpec extends Spec with Mockito {
  import RequestBindingSpec._
  import Param._
  import ValueSource._

  "Request binding" should {
    "successfully bind valid params" ! prop { (first: Option[String], second: Option[String]) =>
      val mockRequest = mock[HttpServletRequest]
      mockRequest.getParameterMap returns Map(
        "first" -> first.map(s => Array(s)).getOrElse(Array())
      ).asJava
      mockRequest.getHeader("second") returns second.orNull
      val fields =
        ("first" ->> queryParam[String]()) ::
        ("second" ->> headerParam[String]().check("oops!")(_ => true)) ::
        HNil

      val results = RequestBinding.bindFromRequest(fields, mockRequest)

      typed[Errors \/ (Option[String], Option[String])](results.map(params =>
        (params.get("first"), params.get("second"))))

      results must beLike {
        case \/-(params) =>
          first.flatMap(blankOption) ==== params.get("first")
          second.flatMap(blankOption) ==== params.get("second")
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
          ("first" ->> queryParam[String]().check("first failed!")(_ => false)) ::
          ("second" ->> headerParam[String]().check("second failed!")(_ => false)) ::
          HNil

        val results = RequestBinding.bindFromRequest(fields, mockRequest)

        typed[Errors \/ (Option[String], Option[String])](results.map(params =>
          (params.get("first"), params.get("second"))))

        results ==== \/.left(NonEmptyList(
          ValidationError("first failed!", FieldName("first")),
          ValidationError("second failed!", FieldName("second"))))
      }
    }

    "fail to compile if an invalid param is accessed" in {
      val mockRequest = mock[HttpServletRequest]
      mockRequest.getParameterMap returns Map.empty[String, Array[String]].asJava
      val fields =
        ("first" ->> queryParam[String]()) ::
        ("second" ->> queryParam[String]()) ::
        HNil

      RequestBinding.bindFromRequest(fields, mockRequest) must beLike {
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
        ("first" ->> queryParam[String]().required(p => s"Hey! '${p.name}' is a required field!")) ::
        HNil

      val expected = first.filterNot(_.isEmpty).toRightDisjunction(NonEmptyList(ValidationError("Hey! 'first' is a required field!", FieldName("first"))))
      expected ==== RequestBinding.bindFromRequest(fields, mockRequest).map(_.get("first"))

    }

    "bind default params" ! prop { (first: Option[String]) =>
      val mockRequest = mock[HttpServletRequest]
      mockRequest.getParameterMap returns Map(
        "first" -> first.map(s => Array(s)).getOrElse(Array.empty)
      ).asJava
      val fields =
        ("first" ->> queryParam[String]().withDefault("<none provided>")) ::
        HNil

      val expected  = \/.right[Errors, String](first.filterNot(_.isEmpty).getOrElse("<none provided>"))
      val result = RequestBinding.bindFromRequest(fields, mockRequest).map(_.get("first"))
      typed[Errors \/ String](result)
      expected ==== result
    }
  }
}

object RequestBindingSpec {
  def blankOption(s: String): Option[String] = if (s.isEmpty) None else Some(s)
}
