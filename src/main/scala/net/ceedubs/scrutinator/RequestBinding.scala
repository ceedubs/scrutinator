package net.ceedubs.scrutinator

import shapeless._
import shapeless.ops.hlist.{ Mapper, Tupler, ZipConst }
import shapeless.ops.record.Keys
import shapeless.record._
import scalaz.syntax.validation._
import scalaz.syntax.std.option._
import scalaz.syntax.id._
import scalaz.syntax.std.string._
import scalaz.syntax.traverse._
import scalaz.Applicative
import shapeless.contrib.scalaz._
import org.scalatra.validation.ValidationError
import scalaz.syntax.std.list._
import scalaz._
import scalaz.Leibniz._

case class NamedParam[A](name: String, Param: A)

@annotation.implicitNotFound("${K} is not a supported type for a field name.")
trait NamedParamConverter[K] {
  def asNamedParam[A](param: A): NamedParam[A]
}

final case class OptionalQueryParam[A](
    description: Option[String] = None/*,
    validation: Kleisli[List, A, ValidationError] = Kleisli[List, A, ValidationError](_ => Nil)*/)

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

// I is the input type for the Kleisli (usually Request)
trait FieldBinder[L <: HList, I] {
  type R <: HList

  def apply(fields: L): Kleisli[ErrorsOr, I, R]
}

object FieldBinder {
  import RequestBinding._

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

object RequestBinding {

  object tuplesToFields extends Poly1 {
    implicit def atTuple[K, V] = at[(K, V)](Function.tupled((key, value) => field[K](value)))
  }

  object bindParam extends Poly1 {
    implicit def atField[K, A, O](implicit npc: NamedParamConverter[K], reader: NamedParamReader[A, Request, O]) = at[FieldType[K, A]] { param =>
      reader.forParam(npc.asNamedParam(param))
    }
  }

  def bindFromRequest[L <: HList](fields: L, request: Request)(implicit fieldBinder: FieldBinder[L, Request]): \/[NonEmptyList[ValidationError], fieldBinder.R] = fieldBinder(fields).run(request).disjunction
}
