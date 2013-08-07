package org.scalatra.contrib.shapeless.binding

import shapeless._

trait ZipConst[C, L <: HList] {
  type Out
  def apply(c: C, l: L): Out
}

trait ZipConstAux[C, L <: HList, O <: HList] {
  def apply(c: C, l: L): O
}

object ZipConst {
  implicit def fromZipConstAux[C, L <: HList, O <: HList](implicit aux: ZipConstAux[C, L, O]): ZipConst[C, L] = new ZipConst[C, L] {
    type Out = O
    def apply(c: C, l: L) = aux(c, l)
  }
}

object ZipConstAux {
  implicit def fromConstMapperAndZip[C, L <: HList, O1 <: HList](implicit constMapper: ConstMapperAux[C, L, O1], zipper: Zip[L :: O1 :: HNil]): ZipConstAux[C, L, zipper.Out] = new ZipConstAux[C, L, zipper.Out] {
    def apply(c: C, l: L) = zipper(l :: constMapper(c, l) :: HNil)
  }
}
