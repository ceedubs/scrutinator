package net.ceedubs.scrutinator

import shapeless._

import scalaz._
import scalaz.syntax.apply._

// this is basically a copy of SequencerAux from scalaz but specialized
// for Kleislis of ValidationNels, since there is an implicit resolution
// problem for those: https://github.com/typelevel/shapeless-contrib/pull/10
// inspired by Travis Brown and Michael Pilquist
// <http://stackoverflow.com/a/16128064>
//sealed trait KleisliValidationNelSequencerAux[A, I <: HList, O <: HList] {
//  def apply(in: I): Kleisli[ErrorsOr, A, O]
//}
//
//trait KleisliValidationNelSequencerAux0 {
//
//  implicit def consSequencerAux[A, B, T <: HList, TO <: HList](
//    implicit seq: KleisliValidationNelSequencerAux[A, T, TO],
//    a: Apply[({ type l[+b] = Kleisli[ErrorsOr, A, B]})#l]
//  ): KleisliValidationNelSequencerAux[A, Kleisli[ErrorsOr, A, B] :: T, B :: TO] = new KleisliValidationNelSequencerAux[A, Kleisli[ErrorsOr, A, B] :: T, B :: TO] {
//    def apply(in: Kleisli[ErrorsOr, A, B] :: T) = in match {
//      case head :: tail =>
//        //Apply[({ type l[+b] = Kleisli[ErrorsOr, A, b]})#l].apply2(head, seq(tail))(_ :: _)
//        //(head |@| seq(tail))(_ :: _)
//        a.apply2(head, seq(tail))
//    }
//  }
//
//}
//
//object KleisliValidationNelSequencerAuxInstances extends KleisliValidationNelSequencerAux0 {
//  type KleisliValidationNel[A, B] = Kleisli[ErrorsOr, A, B]
//
//  implicit def nilSequencerAux[A]: KleisliValidationNelSequencerAux[A, HNil, HNil] = new KleisliValidationNelSequencerAux[A, HNil, HNil] {
//    def apply(in: HNil) =
//      Kleisli[ErrorsOr, A, HNil](_ => Validation.success(HNil))
//  }
//
//  implicit def singleSequencerAux[A, B]: KleisliValidationNelSequencerAux[A, KleisliValidationNel[A, B] :: HNil, B :: HNil] = new KleisliValidationNelSequencerAux[A, KleisliValidationNel[A, B] :: HNil, B :: HNil] {
//    def apply(in: KleisliValidationNel[A, B] :: HNil) = in match {
//      case fa :: _ =>
//        fa.map(_ :: HNil)
//    }
//  }
//
//}
//
////trait SequenceFunctions {
////
////  def sequence[I <: HList, O <: HList, F[_]](in: I)(implicit sequencer: SequencerAux[F, I, O]): F[O] =
////    sequencer(in)
////
////}
//
//// vim: expandtab:ts=2:sw=2
