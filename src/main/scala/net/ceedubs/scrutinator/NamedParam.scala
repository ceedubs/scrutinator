package net.ceedubs.scrutinator

import scalaz._
import shapeless._

case class NamedParam[A](name: String, Param: A)

@annotation.implicitNotFound("${K} is not a supported type for a field name.")
trait NamedParamConverter[K] {
  def asNamedParam[A](param: A): NamedParam[A]
}

object NamedParamConverter {
  implicit def converter[K <: String, A](implicit w: Witness.Aux[K]): NamedParamConverter[K] = new NamedParamConverter[K] {
    def asNamedParam[A](param: A) = NamedParam[A](w.value, param)
  }
}

trait NamedParamReader[T, I, O] {
  def read(input: I, param: NamedParam[T]): ErrorsOr[O]

  def forParam(param: NamedParam[T]): Kleisli[ErrorsOr, I, O] = Kleisli[ErrorsOr, I, O](input => read(input, param))
  
}

object NamedParamReader {
  implicit val optionalStringQueryParamReader: NamedParamReader[OptionalQueryParam[String], Request, Option[String]] = new NamedParamReader[OptionalQueryParam[String], Request, Option[String]] {
    def read(request: Request, param: NamedParam[OptionalQueryParam[String]]) = {
      Validation.success(request.parameters.get(param.name).filterNot(_.isEmpty))
    }
  }
}
