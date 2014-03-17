package net.ceedubs.scrutinator

import org.specs2.mutable._
import org.specs2.ScalaCheck
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

abstract class Spec extends Specification with ScalaCheck with ScrutinatorArb

trait ScrutinatorArb {
  def genParam[A, S <: ValueSource]: Gen[Param[A, S]] = {
    for {
      description <- arbitrary[Option[String]]
      prettyName <- arbitrary[Option[String]]
    } yield Param[A, S](
      description = description,
      prettyName = prettyName)
  }

  implicit def arbParam[A, S <: ValueSource]: Arbitrary[Param[A, S]] = Arbitrary(genParam[A, S])

}
