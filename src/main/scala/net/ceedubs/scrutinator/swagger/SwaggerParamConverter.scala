package net.ceedubs.scrutinator
package swagger

import org.scalatra.swagger.{ AllowableValues, DataType, Model, Parameter }
import ValueSource._
import scalaz._
import scalaz.Leibniz._
import shapeless._
import shapeless.contrib.scalaz._
import shapeless.ops.hlist.ToList
import shapeless.record._

final case class SwaggerModel[L <: HList](modelId: String, fields: L)

trait SwaggerDataTypeConverter[A] {
  def dataType: DataType
}

object SwaggerDataTypeConverter {
  def apply[A](d: DataType): SwaggerDataTypeConverter[A] = new SwaggerDataTypeConverter[A] {
    val dataType = d
  }

  implicit val intSwaggerDataConverter: SwaggerDataTypeConverter[Int] = apply[Int](DataType.Int)
  implicit val stringSwaggerDataConverter: SwaggerDataTypeConverter[String] = apply[String](DataType.String)
  implicit val booleanSwaggerDataConverter: SwaggerDataTypeConverter[Boolean] = apply[Boolean](DataType.Boolean)
  implicit val longSwaggerDataConverter: SwaggerDataTypeConverter[Long] = apply[Long](DataType.Long)
  implicit val floatSwaggerDataConverter: SwaggerDataTypeConverter[Float] = apply[Float](DataType.Float)
  implicit val doubleSwaggerDataConverter: SwaggerDataTypeConverter[Double] = apply[Double](DataType.Double)
  implicit val dateSwaggerDataConverter: SwaggerDataTypeConverter[java.util.Date] = apply[java.util.Date](DataType.Date)

}

trait SwaggerSourceConverter[S <: ValueSource] {
  def sourceType: org.scalatra.swagger.ParamType.ParamType
}

object SwaggerSourceConverter {
  def apply[S <: ValueSource](s: org.scalatra.swagger.ParamType.ParamType): SwaggerSourceConverter[S] = 
    new SwaggerSourceConverter[S] {
      val sourceType = s
    }

  implicit val headerSourceConverter: SwaggerSourceConverter[Headers] =
    SwaggerSourceConverter(org.scalatra.swagger.ParamType.Header)

  implicit val queryStringSourceConverter: SwaggerSourceConverter[QueryString] =
    SwaggerSourceConverter(org.scalatra.swagger.ParamType.Query)

  implicit val jsonBodySourceConverter: SwaggerSourceConverter[Json] =
    SwaggerSourceConverter(org.scalatra.swagger.ParamType.Body)
}

trait SwaggerParamConverter[A] {
  def apply(a: A): ModelState[Parameter]
}

object SwaggerParamConverter extends NamedParamConverters with RequiredParamConverters with ParamWithDefaultConverters {
  def apply[A](f: A => ModelState[Parameter]): SwaggerParamConverter[A] = new SwaggerParamConverter[A] {
    def apply(a: A) = f(a)
  }
}

trait NamedParamConverters {
  implicit def namedParamConverter[A, S <: ValueSource](
      implicit sourceConverter: SwaggerSourceConverter[S], dataTypeConverter: SwaggerDataTypeConverter[A]): SwaggerParamConverter[NamedParam[ParamFromSource[Param[A], S]]] = {
    SwaggerParamConverter[NamedParam[ParamFromSource[Param[A], S]]] { namedParam =>
      State.state(Parameter(
        name = namedParam.name,
        `type` = dataTypeConverter.dataType,
        description = namedParam.param.description,
        notes = namedParam.param.notes,
        paramType = sourceConverter.sourceType,
        defaultValue = None,
        allowableValues = AllowableValues.AnyValue, // TODO
        required = false))
    }
  }

  implicit def swaggerModelParamConverter[L <: HList, S <: ValueSource](implicit modelConverter: SwaggerModelConverter[SwaggerModel[L]], sourceConverter: SwaggerSourceConverter[S]): SwaggerParamConverter[NamedParam[ParamFromSource[SwaggerModel[L], S]]] =
    SwaggerParamConverter[NamedParam[ParamFromSource[SwaggerModel[L], S]]]{ namedParam =>
      for {
        model <- modelConverter(namedParam.param)
      } yield Parameter(
        name = namedParam.name,
        `type` = DataType(model.id),
        description = None, // TODO
        notes = None, // TODO
        paramType = sourceConverter.sourceType,
        defaultValue = None,
        allowableValues = AllowableValues.AnyValue, // TODO
        required = false)
    }
 
}

trait RequiredParamConverters {
  implicit def namedRequiredParamConverter[A, S <: ValueSource](implicit converter: SwaggerParamConverter[NamedParam[ParamFromSource[A, S]]]): SwaggerParamConverter[NamedParam[ParamFromSource[RequiredParam[A], S]]] = {
    SwaggerParamConverter[NamedParam[ParamFromSource[RequiredParam[A], S]]] { namedRequiredParam =>
      val namedInnerParam = NamedParam[ParamFromSource[A, S]](
        namedRequiredParam.name,
        ParamFromSource[A, S](namedRequiredParam.param.param))
      converter(namedInnerParam).map(_.copy(required = true))
    }
  }
}

trait ParamWithDefaultConverters {
  implicit def ParamWithDefaultConverter[A, S <: ValueSource](implicit converter: SwaggerParamConverter[NamedParam[ParamFromSource[Param[A], S]]], showA: SwaggerShow[A]): SwaggerParamConverter[NamedParam[ParamFromSource[ParamWithDefault[A], S]]] = {
    SwaggerParamConverter[NamedParam[ParamFromSource[ParamWithDefault[A], S]]] { namedParamWithDefault =>
      val namedInnerParam = NamedParam[ParamFromSource[Param[A], S]](
        namedParamWithDefault.name,
        ParamFromSource[Param[A], S](namedParamWithDefault.param.param))
      converter(namedInnerParam).map(_.copy(
        defaultValue = Some(showA.shows(
          SwaggerSpec(namedParamWithDefault.param.default)))))
    }
  }
}

trait FieldListSwaggerConverter[L <: HList] {
  def apply(fields: L): ModelState[Seq[Parameter]]
}

object FieldListSwaggerConverter {

  implicit def toSwaggerParamConverter[F[_], L <: HList, O <: HList](implicit traverser: TraverserAux[L, toSwaggerParam.type, F, O], ev: F[O] === ModelState[O], toList: ToList[O, Parameter]): FieldListSwaggerConverter[L] = new FieldListSwaggerConverter[L] {
    def apply(fields: L) = ev(traverser(fields)).map(toList.apply)
  }

  def toSwaggerParams[L <: HList](fields: L)(implicit converter: FieldListSwaggerConverter[L]): ModelState[Seq[Parameter]] =
    converter(fields)
}

object toSwaggerParam extends Poly1 {
  implicit def swaggerParam[K, P](implicit namedParamConverter: NamedParamConverter[K], swaggerConverter: SwaggerParamConverter[NamedParam[P]]) = at[FieldType[K, P]] { param =>
    val namedParam: NamedParam[P] = namedParamConverter.asNamedParam(param)
    swaggerConverter(namedParam) }
}
