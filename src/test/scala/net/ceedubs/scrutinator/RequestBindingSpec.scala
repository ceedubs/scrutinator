package net.ceedubs.scrutinator

import org.specs2.mutable._
import org.specs2.mock.Mockito
import org.specs2.ScalaCheck
import shapeless._
import shapeless.syntax.singleton._
import shapeless.test.illTyped
import scalaz._
import scalaz.syntax.std.option._
import javax.servlet.http.HttpServletRequest
import scala.collection.JavaConverters._
import org.scalatra.validation.{ FieldName, ValidationError }

class RequestBindingSpec extends Specification with Mockito with ScalaCheck {
  import RequestBindingSpec._

  "Request binding" should {
    "successfully bind valid params" ! prop { (first: Option[String], second: Option[String]) =>
      val mockRequest = mock[HttpServletRequest]
      mockRequest.getParameterMap returns Map(
        "first" -> first.map(s => Array(s)).getOrElse(Array()),
        "second" -> second.map(s => Array(s)).getOrElse(Array())
      ).asJava
      val fields =
        ("first" ->> QueryParam[String]()) ::
        ("second" ->> QueryParam[String]()) ::
        HNil

      RequestBinding.bindFromRequest(fields, mockRequest) must beLike {
        case \/-(params) =>
          params.get("first") must be_===(first.flatMap(blankOption))
          params.get("second") must be_===(second.flatMap(blankOption))
      }
    }

    "fail to compile if an invalid param is accessed" in {
      val mockRequest = mock[HttpServletRequest]
      mockRequest.getParameterMap returns Map.empty[String, Array[String]].asJava
      val fields =
        ("first" ->> QueryParam[String]()) ::
        ("second" ->> QueryParam[String]()) ::
        HNil

      RequestBinding.bindFromRequest(fields, mockRequest) must beLike {
        case \/-(params) =>
          params.get("first") // compiles
          params.get("second") // compiles
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
        ("first" ->> RequiredParam(QueryParam[String](), (p: NamedParam[_]) => s"Hey! '${p.name}' is a required field!")) ::
        HNil

      val expected = first.filterNot(_.isEmpty).map(_ :: HNil).toRightDisjunction(NonEmptyList(ValidationError("Hey! 'first' is a required field!", FieldName("first"))))
      RequestBinding.bindFromRequest(fields, mockRequest) must be_===(expected)
    }
  }
}

object RequestBindingSpec {
  def blankOption(s: String): Option[String] = if (s.isEmpty) None else Some(s)
}
