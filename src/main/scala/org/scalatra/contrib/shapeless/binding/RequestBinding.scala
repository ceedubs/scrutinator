package org.scalatra.contrib.shapeless.binding

import shapeless._
import shapeless.ops.hlist.{ Mapper, Tupler, ZipConst }
import scalaz.syntax.validation._
import scalaz.syntax.std.option._
import scalaz.syntax.id._
import scalaz.syntax.std.string._
import scalaz.syntax.traverse._
import scalaz.Applicative
import shapeless.contrib.scalaz._
import org.scalatra.validation.ValidationError
import scalaz.syntax.std.list._

trait RequestParamReader[A] {
  type Source <: ValueSource
  def read(key: String, request: Request): Option[ErrorsOr[A]]
}

object RequestParamReader {
  type Aux[A, S] = RequestParamReader[A] { type Source = S }

  type QueryParamReader[A] = RequestParamReader[A] { type Source = ValueSource.QueryString }

  implicit val stringQueryParamReader: QueryParamReader[String] = new RequestParamReader[String] {
    type Source = ValueSource.QueryString
    def read(key: String, request: Request) = request.parameters.get(key).filterNot(_.isEmpty).map(_.success)
  }

  implicit val intQueryParamReader: QueryParamReader[Int] = new RequestParamReader[Int] {
    type Source = ValueSource.QueryString
    def read(key: String, request: Request) = {
      request.parameters.get(key).map(x => x.parseInt.leftMap(_ => ValidationError(s"$x is not a valid integer").wrapNel))
    }
  }
}

sealed trait ValueSource {
  def name: String
}

object ValueSource {
  trait Header extends ValueSource
  implicit val Header: Header = new Header {
    val name = "Header"
  }

  trait QueryString extends ValueSource
  implicit val QueryString: QueryString = new QueryString {
    val name = "QueryString"
  }

  trait Body extends ValueSource
  implicit val Body: Body = new Body {
    val name = "Body"
  }
}

case class RequestField[Value, Source](
    name: String,
    source: Source,
    required: Boolean,
    description: Option[String],
    validations: List[Value => Option[ValidationError]])

case class RequestBinding[Value, Output, Source](
    field: RequestField[Value, Source],
    reader: RequestParamReader[Value],
    validator: BoundValueValidator[Value, Output])

case class RequestBindingWithRequest[Value, Output, Source](
    binding: RequestBinding[Value, Output, Source],
    request: Request)

trait BoundValueValidator[Value, Output] {
  def runValidations(boundValue: Option[ErrorsOr[Value]], validations: List[Value => Option[ValidationError]]): ErrorsOr[Output]
}

// TODO DRY
trait RequiredValueBoundValueValidator[Value] extends BoundValueValidator[Value, Value] {
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

case class DefaultValueBoundValueValidator[Value](defaultValue: Value) extends BoundValueValidator[Value, Value] {

  def runValidations(boundValue: Option[ErrorsOr[Value]], validations: List[Value => Option[ValidationError]]): ErrorsOr[Value] = {
    boundValue.getOrElse(defaultValue.successNel) flatMap { value =>
      validations.map(_(value)).flatten.toNel.toFailure(value)
    }
  }
}

class OptionalValueBoundValueValidator[Value] extends BoundValueValidator[Value, Option[Value]] {

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
  implicit def default[V, O, S] = at[RequestBindingWithRequest[V, O, S]]{ bindingWithReq =>
    val boundValue = bindingWithReq.binding.reader.read(bindingWithReq.binding.field.name, bindingWithReq.request)
    bindingWithReq.binding.validator.runValidations(boundValue, bindingWithReq.binding.field.validations): ErrorsOr[O]
  }
}

object tupleToBindingWithRequest extends Poly1 {
  implicit def default[V, O, S] = at[(RequestBinding[V, O, S], Request)]{ t =>
    val bindingWithRequest: RequestBindingWithRequest[V, O, S] = RequestBindingWithRequest[V, O, S](
      binding = t._1,
      request = t._2)
    bindingWithRequest
  }
}

sealed trait RequestFieldBinder[L <: HList] {
  type Out <: HList
  def apply(request: Request, fields: L): Out
}

object RequestFieldBinder {
  type Aux[L <: HList, O <: HList] = RequestFieldBinder[L] { type Out = O }
  implicit def fromZipConstAndMapper[L <: HList, O1 <: HList, O <: HList](implicit zipConster: ZipConst.Aux[Request, L, O1], bindingMapper: Mapper.Aux[tupleToBindingWithRequest.type, O1, O]): Aux[L, O] = new RequestFieldBinder[L] {
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

  def requiredParam[A, S <: ValueSource](fieldName: String)(implicit source: S, reader: RequestParamReader.Aux[A, S]): RequestBinding[A, A, S] = {
    RequestBinding[A, A, S](
      field = RequestField(
        name = fieldName,
        source = source,
        required = true,
        description = None,
        validations = Nil),
      reader = reader,
      validator = new RequiredValueBoundValueValidator[A] { val requiredError = ValidationError(s"$fieldName is required") }) // TODO improve error message handling
  }

  def optionalParam[A, S <: ValueSource](fieldName: String)(implicit source: S, reader: RequestParamReader.Aux[A, S]): RequestBinding[A, Option[A], S] = {
    RequestBinding[A, Option[A], S](
      field = RequestField(
        name = fieldName,
        source = source,
        required = false,
        description = None,
        validations = Nil),
      reader = reader,
      validator = new OptionalValueBoundValueValidator[A])
  }

  def paramWithDefault[A, S <: ValueSource](fieldName: String, defaultValue: A)(implicit source: S, reader: RequestParamReader.Aux[A, S]): RequestBinding[A, A, S] = {
    RequestBinding[A, A, S](
      field = RequestField(
        name = fieldName,
        source = source,
        required = false,
        description = None,
        validations = Nil),
      reader = reader,
      validator = new DefaultValueBoundValueValidator[A](defaultValue)) // TODO improve error message handling
  }

}
