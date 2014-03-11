package net.ceedubs.scrutinator

import org.specs2.mutable._
import org.specs2.mock.Mockito
import org.specs2.ScalaCheck
import shapeless._
import shapeless.syntax.singleton._
import shapeless.test.illTyped
import scalaz.\/-
import javax.servlet.http.HttpServletRequest
import scala.collection.JavaConverters._

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
        ("first" ->> OptionalQueryParam[String]()) ::
        ("second" ->> OptionalQueryParam[String]()) ::
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
        ("first" ->> OptionalQueryParam[String]()) ::
        ("second" ->> OptionalQueryParam[String]()) ::
        HNil

      RequestBinding.bindFromRequest(fields, mockRequest) must beLike {
        case \/-(params) =>
          params.get("first") // compiles
          params.get("second") // compiles
          illTyped("""params.get("not-a-param")""") // wouldn't compile
          ok // as long as this test compiles, it should pass
      }
    }
  }
}

object RequestBindingSpec {
  def blankOption(s: String): Option[String] = if (s.isEmpty) None else Some(s)
}
