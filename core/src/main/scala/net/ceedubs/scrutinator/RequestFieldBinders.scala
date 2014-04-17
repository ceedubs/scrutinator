package net.ceedubs.scrutinator

import net.ceedubs.scrutinator.readers.ParamReader
import shapeless._
import shapeless.record._

trait RequestFieldBinders {
  implicit def requestFieldBinder[L <: HList](implicit strategy: FieldBindingStrategy[L, Request, bindParamFromRequest.type]): FieldBinder.Aux[L, Request, strategy.R] = strategy.fieldBinder
}

object bindParamFromRequest extends Poly1 {
  implicit def atField[K, A, O](implicit npc: NamedParamConverter[K], reader: ParamReader[Validated, (NamedParam[A], Request), O]) = at[FieldType[K, A]] { param =>
    val namedParam: NamedParam[A] = npc.asNamedParam(param)
    reader.local((request: Request) => (namedParam, request)).reader
  }
}
