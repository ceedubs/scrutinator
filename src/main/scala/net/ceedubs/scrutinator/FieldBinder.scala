package net.ceedubs.scrutinator

import shapeless._
import shapeless.record._
import shapeless.ops.record.Keys
import shapeless.ops.hlist.{ Mapper, Tupler, ZipConst }
import shapeless.contrib.scalaz._
import scalaz._
import scalaz.Leibniz._

// I is the input type for the Kleisli (usually Request)
trait FieldBinder[L <: HList, I] {
  type R <: HList

  def apply(fields: L): Kleisli[ErrorsOr, I, R]
}

object FieldBinder {

  object tuplesToFields extends Poly1 {
    implicit def atTuple[K, V] = at[(K, V)](Function.tupled((key, value) => field[K](value)))
  }

  object bindParam extends Poly1 {
    implicit def atField[K, A, O](implicit npc: NamedParamConverter[K], reader: NamedParamReader[A, Request, O]) = at[FieldType[K, A]] { param =>
      reader.forParam(npc.asNamedParam(param))
    }
  }

  type Aux[L <: HList, I, R0 <: HList] = FieldBinder[L, I] {
    type R = R0
  }

  implicit def fieldBinder[L <: HList, K <: HList, I, O1 <: HList, F0[_], O2 <: HList, F1[_], O3 <: HList, O4 <: HList](implicit
      keys: Keys.Aux[L, K],
      bindParamTraverser: TraverserAux[L, bindParam.type, F0, O1],
      fEv: UnapplyAux[Functor, F0[O1], F1, O2],
      zipper: shapeless.ops.hlist.Zip.Aux[K :: O2 :: HNil, O3],
      fieldMapper: Mapper.Aux[tuplesToFields.type, O3, O4],
      lEv: F1[O4] === Kleisli[ErrorsOr, I, O4]): FieldBinder.Aux[L, I, O4] = new FieldBinder[L, I] {

    type R = O4

    def apply(fields: L): Kleisli[ErrorsOr, I, O4] = {
      val bindingsReader = bindParamTraverser(fields)
      val result = fEv.TC.map(fEv(bindingsReader)) { boundValues =>
        val tupled = zipper(keys() :: boundValues :: HNil) 
        fieldMapper(tupled)
      }
      witness(lEv)(result)
    }
  }
}
