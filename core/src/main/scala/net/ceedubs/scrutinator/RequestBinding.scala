package net.ceedubs.scrutinator

import shapeless._
import scalaz._

object RequestBinding {

  def fieldBinder[L <: HList](fields: L)(implicit binder: FieldBinder[L, Request]): Kleisli[ErrorsOr, Request, binder.R] =
    binder(fields).
    local((req: Request) => (Nil, req)).
    mapK[ErrorsOr, binder.R](_.disjunction)

}
