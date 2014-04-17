package net.ceedubs.scrutinator

import shapeless._
import shapeless.record._
import shapeless.ops.record.Keys
import shapeless.ops.hlist.ZipWithKeys
import shapeless.contrib.scalaz._
import scalaz._
import scalaz.Leibniz._

/** I is the input type for the Kleisli (usually Request) */
trait FieldBinder[L <: HList, I] {
  type R <: HList

  def apply(fields: L): Kleisli[Validated, (CursorHistory, I), R]
}

object FieldBinder extends RequestFieldBinders {

  type Aux[L <: HList, I, R0 <: HList] = FieldBinder[L, I] {
    type R = R0
  }

  def apply[L <: HList, I](implicit fieldBinder: FieldBinder[L, I]): Aux[L, I, fieldBinder.R] = fieldBinder

}

object tuplesToFields extends Poly1 {
  implicit def atTuple[K, V] = at[(K, V)](Function.tupled((key, value) => field[K](value)))
}

trait FieldBindingStrategy[L <: HList, I, P <: Poly] {
  type R <: HList

  def fieldBinder: FieldBinder.Aux[L, I, R]

  def apply(fields: L): Kleisli[Validated, (CursorHistory, I), R] = fieldBinder(fields)
}

object FieldBindingStrategy {

  type Aux[L <: HList, I, P <: Poly, R0 <: HList] = FieldBindingStrategy[L, I, P] {
    type R = R0
  }

  implicit def polyTraverserStrategy[L <: HList, K <: HList, I, P <: Poly, O1 <: HList, F0[_], O2 <: HList, F1[_], O3 <: HList](implicit
      keys: Keys.Aux[L, K],
      bindParamTraverser: TraverserAux[L, P, F0, O1],
      fEv: UnapplyAux[Functor, F0[O1], F1, O2],
      zipWithKeys: ZipWithKeys.Aux[K, O2, O3],
      lEv: F1[O3] === Kleisli[Validated, (CursorHistory, I), O3]): FieldBindingStrategy.Aux[L, I, P, O3] = new FieldBindingStrategy[L, I, P] {

    type R = O3

    val fieldBinder = new FieldBinder[L, I] {
      type R = O3

      def apply(fields: L): Kleisli[Validated, (CursorHistory, I), O3] = {
        val bindingsReader = bindParamTraverser(fields)
        val result = fEv.TC.map(fEv(bindingsReader)) { boundValues =>
          zipWithKeys(keys(), boundValues)
        }
        witness(lEv)(result)
      }
    }
  }
}
