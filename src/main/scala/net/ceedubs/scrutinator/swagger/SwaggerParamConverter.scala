package net.ceedubs.scrutinator
package swagger

import net.ceedubs.scrutinator.json4s.readers.JsonObjectParam
import shapeless._
import org.scalatra.swagger.{ AllowableValues, DataType, Model, Parameter }
import ValueSource._
import scalaz._

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
      implicit sourceConverter: SwaggerSourceConverter[S], dataTypeConverter: SwaggerDataTypeConverter[A]): SwaggerParamConverter[NamedParam[Param[A, S]]] = {
    SwaggerParamConverter[NamedParam[Param[A, S]]] { namedParam =>
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

  //implicit def namedJsonObjectParamConverter[L <: HList](implicit sourceConverter: SwaggerSourceConverter[ValueSource.Json]): SwaggerParamConverter[NamedParam[JsonObjectParam[L]]] = ???

  implicit def namedJsonObjectParamConverter[L <: HList](implicit modelConverter: SwaggerModelConverter[NamedParam[JsonObjectParam[L]]], sourceConverter: SwaggerSourceConverter[ValueSource.Json]): SwaggerParamConverter[NamedParam[JsonObjectParam[L]]] =
    SwaggerParamConverter[NamedParam[JsonObjectParam[L]]]{ namedParam =>
      for {
        model <- modelConverter(namedParam)
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
  implicit def namedRequiredParamConverter[A](implicit converter: SwaggerParamConverter[NamedParam[A]]): SwaggerParamConverter[NamedParam[RequiredParam[A]]] = {
    SwaggerParamConverter[NamedParam[RequiredParam[A]]] { namedRequiredParam =>
      val namedInnerParam = NamedParam[A](namedRequiredParam.name, namedRequiredParam.param.param)
      converter(namedInnerParam).map(_.copy(required = true))
    }
  }
}

trait ParamWithDefaultConverters {
  implicit def ParamWithDefaultConverter[A, S <: ValueSource](implicit converter: SwaggerParamConverter[NamedParam[Param[A, S]]], showA: SwaggerShow[A]): SwaggerParamConverter[NamedParam[ParamWithDefault[A, S]]] = {
    SwaggerParamConverter[NamedParam[ParamWithDefault[A, S]]] { namedParamWithDefault =>
      val namedInnerParam = NamedParam[Param[A, S]](namedParamWithDefault.name, namedParamWithDefault.param.param)
      converter(namedInnerParam).map(_.copy(
        defaultValue = Some(showA.shows(
          SwaggerSpec(namedParamWithDefault.param.default)))))
    }
  }
}
