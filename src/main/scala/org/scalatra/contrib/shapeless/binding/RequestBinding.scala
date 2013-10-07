package org.scalatra.contrib.shapeless.binding

import shapeless._
import scalaz.syntax.validation._
import scalaz.syntax.std.option._
import scalaz.syntax.id._
import scalaz.syntax.std.string._
import scalaz.syntax.traverse._
import scalaz.Applicative
import shapeless.contrib.scalaz._
import org.scalatra.validation.ValidationError
import scalaz.syntax.std.list._
import org.scalatra.contrib.shapeless.binding.RequestParamReader.QueryParamReader

// a simplified mock HTTP request :)
trait Request {
  def queryParams: Map[String, String]
}

trait RequestParamReader[A] {
  type Source <: ValueSource
  def read(key: String, request: Request): Option[ErrorsOr[A]]
}

object RequestParamReader {
  type QueryParamReader[A] = RequestParamReader[A] { type Source = ValueSource.QueryString }
}

object QueryParamReader {
  implicit val stringQueryParamReader: QueryParamReader[String] = new RequestParamReader[String] {
    type Source = ValueSource.QueryString
    def read(key: String, request: Request) = request.queryParams.get(key).filterNot(_.isEmpty).map(_.success)
  }

  implicit val intQueryParamReader: QueryParamReader[Int] = new RequestParamReader[Int] {
    type Source = ValueSource.QueryString
    def read(key: String, request: Request) = {
      request.queryParams.get(key).map(x => x.parseInt.leftMap(_ => ValidationError(s"$x is not a valid integer").wrapNel))
    }
  }
}

trait RequestKey {
  type Source <: ValueSource

  def name: String

  def source: Source
}

sealed trait ValueSource
object ValueSource {
  trait Header extends ValueSource
  case object Header extends Header

  trait QueryString extends ValueSource
  case object QueryString extends QueryString

  trait Body extends ValueSource
  case object Body extends Body
}

trait RequestField {
  type Value
  def key: RequestKey
  def required: Boolean
  def description: Option[String]
  def validations: List[Value => Option[ValidationError]]
}

object RequestField {
  type Aux[V] = RequestField { type Value = V}
}

trait RequestBinding { self =>
  type Value
  type Output

  def field: RequestField.Aux[Value]
  def reader: RequestParamReader[Value]
  def validator: BoundValueValidator.Aux[Value, Output]
}

object RequestBinding {
  type Aux[V, O] = RequestBinding {
    type Value = V
    type Output = O
  }
}

trait RequestBindingWithRequest { self =>
  type Value
  type Output

  def binding: RequestBinding.Aux[Value, Output]

  def request: Request
}

object RequestBindingWithRequest { self =>
  type Aux[V, O] = RequestBindingWithRequest {
    type Value = V
    type Output = O
  }
}

trait BoundValueValidator {
  type Value
  type Output

  def runValidations(boundValue: Option[ErrorsOr[Value]], validations: List[Value => Option[ValidationError]]): ErrorsOr[Output]
}

object BoundValueValidator {
  type Aux[V, O] = BoundValueValidator {
    type Value = V
    type Output = O
  }
}

// TODO DRY
trait RequiredValueBoundValueValidator extends BoundValueValidator {
  type Output = Value
  def requiredError: ValidationError

  def runValidations(boundValue: Option[ErrorsOr[Value]], validations: List[Value => Option[ValidationError]]): ErrorsOr[Value] = {
    boundValue.cata(
      some = { errorsOrValue =>
        errorsOrValue flatMap { value =>
          validations.map(_(value)).flatten.toNel.toFailure(value)
        }
      },
      none = requiredError.failNel
    )
  }
}

trait DefaultValueBoundValueValidator extends BoundValueValidator {
  type Output = Value
  def defaultValue: Value

  def runValidations(boundValue: Option[ErrorsOr[Value]], validations: List[Value => Option[ValidationError]]): ErrorsOr[Value] = {
    boundValue.getOrElse(defaultValue.successNel) flatMap { value =>
      validations.map(_(value)).flatten.toNel.toFailure(value)
    }
  }
}

trait OptionalValueBoundValueValidator extends BoundValueValidator {
  type Output = Option[Value]

  def runValidations(boundValue: Option[ErrorsOr[Value]], validations: List[Value => Option[ValidationError]]): ErrorsOr[Option[Value]] = {
    boundValue.cata(
      some = { errorsOrValue =>
        errorsOrValue flatMap { value =>
          validations.map(_(value)).flatten.toNel.toFailure(value.some)
        }
      },
      none = None.successNel
    )
  }
}

object bindFromReq extends Poly1 {
  implicit def default[V, O] = at[RequestBindingWithRequest.Aux[V, O]]{ bindingWithReq =>
    val boundValue = bindingWithReq.binding.reader.read(bindingWithReq.binding.field.key.name, bindingWithReq.request)
    bindingWithReq.binding.validator.runValidations(boundValue, bindingWithReq.binding.field.validations): ErrorsOr[O]
  }
}

object tupleToBindingWithRequest extends Poly1 {
  implicit def default[V, O] = at[(RequestBinding.Aux[V, O], Request)]{ t =>
    val bindingWithRequest: RequestBindingWithRequest.Aux[V, O] = new  RequestBindingWithRequest {
      type Value = V
      type Output = O

      val request: Request = t._2

      val binding: RequestBinding.Aux[Value, Output] = t._1
    }
    bindingWithRequest
  }
}

sealed trait RequestFieldBinder[L <: HList] {
  type Out <: HList
  def apply(request: Request, fields: L): Out
}

object RequestFieldBinder {
  type Aux[L <: HList, O <: HList] = RequestFieldBinder[L] { type Out = O }
  implicit def fromZipConstAndMapper[L <: HList, O1 <: HList, O <: HList](implicit zipConster: ZipConstAux[Request, L, O1], bindingMapper: MapperAux[tupleToBindingWithRequest.type, O1, O]): Aux[L, O] = new RequestFieldBinder[L] {
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

object RequestBindingSupport {
  def bindAllFromRequest[T, L <: HList, F[_], O <: HList, TO](fields: T)(implicit request: Request, gen: Generic.Aux[T, L], bindingTraverser: RequestBindingTraverser.Aux[L, F, O], tupler: Tupler[O], applicative: Applicative[F]): F[tupler.Out] = {
    val traversed = bindingTraverser(request, gen.to(fields))
    applicative.map(traversed)(tupler(_))
  }
}