package org.scalatra.contrib.shapeless.binding

import shapeless._
import scalaz.syntax.validation._
import scalaz.syntax.std.string._
import scalaz.{Applicative, ValidationNel}
import shapeless.contrib.scalaz.TraverseFunctions.TraverserAux

// a simplified mock HTTP request :)
trait Request {
  def queryParams: Map[String, String]
}

//sealed trait RequestParamSource
//trait QueryParam extends RequestParamSource

trait RequestParamReader[A] {
  def read(key: String, request: Request): Option[A]
}

trait QueryParamReader[A] extends RequestParamReader[A]

object QueryParamReader {
  implicit val stringQueryParamReader: QueryParamReader[String] = new QueryParamReader[String] {
    def read(key: String, request: Request) = request.queryParams.get(key).filterNot(_.isEmpty)
  }

  implicit val intQueryParamReader: QueryParamReader[Int] = new QueryParamReader[Int] {
    def read(key: String, request: Request) = {
      request.queryParams.get(key).flatMap(_.parseInt.toOption)
    }
  }
}

object bindFromReq extends Poly1 {
  implicit def default[A] = at[RequestBinding[A]]{ requestBinding =>
    requestBinding.field.reader.read(requestBinding.field.key, requestBinding.request)
  }
}

object tupleToBinding extends Poly1 {
  implicit def default[T] = at[(RequestField[T], Request)](t => new  RequestBinding(t._1, t._2))
}

final case class RequestField[T](key: String, reader: RequestParamReader[T])

sealed trait RequestFieldBinder[L <: HList] {
  type Out <: HList
  def apply(request: Request, fields: L): Out
}

object RequestFieldBinder {
  type Aux[L <: HList, O <: HList] = RequestFieldBinder[L] { type Out = O }
  implicit def fromZipConstAndMapper[L <: HList, O1 <: HList, O <: HList](implicit zipConster: ZipConstAux[Request, L, O1], bindingMapper: MapperAux[tupleToBinding.type, O1, O]): Aux[L, O] = new RequestFieldBinder[L] {
    type Out = O
    def apply(request: Request, fields: L) = {
      val tuples = zipConster(request, fields)
      bindingMapper(tuples)
    }
  }
}

sealed trait RequestBindingTraverser[L <: HList] {
  type Out
  def apply(request: Request, fields: L): Out
}

object RequestBindingTraverser {
  type Aux[L <: HList, F[_], O <: HList] = RequestBindingTraverser[L] { type Out = F[O] }

  implicit def fromBinderAndTraverser[L <: HList, O1 <: HList, F[_], O <: HList](implicit binder: RequestFieldBinder.Aux[L, O1], traverser: TraverserAux[O1, bindFromReq.type, F, O]): Aux[L, F, O] = new RequestBindingTraverser[L] {
    type Out = F[O]
    def apply(request: Request, fields: L) = {
      val bindings = binder(request, fields)
      traverser(bindings)
    }
  }

}

final case class RequestBinding[A](field: RequestField[A], request: Request)

object RequestBinding {
  type ValidationErrorsOr[A] = ValidationNel[String, A]

  def queryParam[A](key: String)(implicit reader: QueryParamReader[A]): RequestField[A] = new RequestField[A](key, reader)

  def bindAllFromRequest[T, L <: HList, F[_], O <: HList, TO](fields: T)(implicit request: Request, gen: Generic.Aux[T, L], bindingTraverser: RequestBindingTraverser.Aux[L, F, O], tupler: Tupler[O], applicative: Applicative[F]): F[tupler.Out] = {
    val traversed = bindingTraverser(request, gen.to(fields))
    applicative.map(traversed)(tupler(_))
  }
}