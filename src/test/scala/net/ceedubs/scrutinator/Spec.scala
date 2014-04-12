package net.ceedubs.scrutinator

import scalaz._
import org.specs2.mutable._
import org.specs2.ScalaCheck
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.specs2.mock.Mockito
import javax.servlet.http.HttpServletRequest
import java.io.ByteArrayInputStream
import javax.servlet.ServletInputStream

abstract class Spec extends Specification
    with ScalaCheck
    with ScrutinatorArb
    with SpecHelpers
    with Mockito {

  def mockRequest(jsonBody: Option[String]): HttpServletRequest = {
    val req = mock[HttpServletRequest]
    jsonBody foreach { s =>
      req.getCharacterEncoding returns "UTF-8"
      req.getContentType returns "applications/json"
      req.getInputStream returns new ServletInputStream {
        val inputStream = new ByteArrayInputStream(s.getBytes("UTF-8"))

        override def read(): Int = inputStream.read()

        override def close() {
          super.close()
          inputStream.close()
        }
      }
    }
    req
  }
   
}

case object ForcedError extends ParamError

trait SpecHelpers {
  def typed[A](a: => A) {}

  def urlEncode(s: String): String = java.net.URLEncoder.encode(s, "UTF-8").replace("+", "%20")
}

trait ScrutinatorArb {
  def genParam[A]: Gen[Field[A]] = {
    for {
      description <- arbitrary[Option[String]]
      notes <- arbitrary[Option[String]]
      prettyName <- arbitrary[Option[String]]
    } yield Field[A](
      description = description,
      notes = notes,
      prettyName = prettyName)
  }

  implicit def arbParam[A]: Arbitrary[Field[A]] = Arbitrary(genParam[A])

  def genParamFromSource[P, S <: ValueSource](implicit pArb: Arbitrary[P]): Gen[ParamFromSource[P, S]] = {
    for {
      param <- pArb.arbitrary
    } yield ParamFromSource[P, S](param)
  }

  def arbParamFromSource[P : Arbitrary, S <: ValueSource]: Arbitrary[ParamFromSource[P, S]] = Arbitrary(genParamFromSource[P, S])

  implicit def arbQueryParam[P : Arbitrary]: Arbitrary[QueryParam[P]] = Arbitrary(genParamFromSource[P, ValueSource.QueryString])

  implicit def arbHeaderParam[P : Arbitrary]: Arbitrary[HeaderParam[P]] = Arbitrary(genParamFromSource[P, ValueSource.Headers])

  implicit def arbPathParam[P : Arbitrary]: Arbitrary[PathParam[P]] = Arbitrary(genParamFromSource[P, ValueSource.Path])

  def genRequiredParam[P](implicit arbP: Arbitrary[P]): Gen[RequiredParam[P]] = {
    for {
      param <- arbP.arbitrary
      errorMsg <- arbitrary[String]
    } yield RequiredParam[P](param, _ => errorMsg)
  }

  implicit def arbRequiredParam[P : Arbitrary]: Arbitrary[RequiredParam[P]] = Arbitrary(genRequiredParam[P])

  def genParamWithDefault[A](implicit arbA: Arbitrary[A]): Gen[FieldWithDefault[A]] = {
    for {
      default <- arbA.arbitrary
      param <- genParam[A]
    } yield FieldWithDefault(param, default)
  }

  implicit def arbParamWithDefault[A : Arbitrary]: Arbitrary[FieldWithDefault[A]] = Arbitrary(genParamWithDefault[A])

  // there is a better implementation in scalaz-scalacheck-binding,
  // but we have a Scalacheck version mismatch :\
  implicit def NonEmptyListArbitrary[A](implicit arbA: Arbitrary[A]): Arbitrary[NonEmptyList[A]] = 
    Arbitrary(for {
      a <- arbA.arbitrary
      aList <- arbitrary[List[A]]
    } yield NonEmptyList.nel(a, aList))
}
