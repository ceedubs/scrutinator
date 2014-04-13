package net.ceedubs.scrutinator

import shapeless._

final case class Model[L <: HList](
  fields: L,
  description: Option[String] = None)

final case class ModelWithId[L <: HList](
  id: String,
  model: Model[L])

trait AsModel[M] {
  type L <: HList
  def apply(m: M): Model[L]
}

object AsModel {
  type Aux[M, L0 <: HList] = AsModel[M] { type L = L0 }

  def asModel[M, L0 <: HList](f: M => Model[L0]): AsModel.Aux[M, L0] =
    new AsModel[M] {
      type L = L0
      def apply(m: M) = f(m)
    }

  implicit def modelAsModel[L <: HList]: AsModel.Aux[Model[L], L] =
    asModel[Model[L], L](identity)

  implicit def modelWithIdAsModel[L <: HList]: AsModel.Aux[ModelWithId[L], L] =
    asModel[ModelWithId[L], L](_.model)
}
