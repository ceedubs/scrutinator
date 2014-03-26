package net.ceedubs.scrutinator

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

trait SpecHelpers {
  def typed[A](a: => A) {}
}

trait ScrutinatorArb {
  def genParam[A, S <: ValueSource]: Gen[Param[A, S]] = {
    for {
      description <- arbitrary[Option[String]]
      notes <- arbitrary[Option[String]]
      prettyName <- arbitrary[Option[String]]
    } yield Param[A, S](
      description = description,
      notes = notes,
      prettyName = prettyName)
  }

  implicit def arbParam[A, S <: ValueSource]: Arbitrary[Param[A, S]] = Arbitrary(genParam[A, S])

  def genRequiredParam[P](implicit arbP: Arbitrary[P]): Gen[RequiredParam[P]] = {
    for {
      param <- arbP.arbitrary
      errorMsg <- arbitrary[String]
    } yield RequiredParam[P](param, _ => errorMsg)
  }

  implicit def arbRequiredParam[P : Arbitrary]: Arbitrary[RequiredParam[P]] = Arbitrary(genRequiredParam[P])

  def genParamWithDefault[A, S <: ValueSource](implicit arbA: Arbitrary[A]): Gen[ParamWithDefault[A, S]] = {
    for {
      default <- arbA.arbitrary
      param <- genParam[A, S]
    } yield ParamWithDefault[A, S](param, default)
  }

  implicit def arbParamWithDefault[A : Arbitrary, S <: ValueSource]: Arbitrary[ParamWithDefault[A, S]] = Arbitrary(genParamWithDefault[A, S])
}
