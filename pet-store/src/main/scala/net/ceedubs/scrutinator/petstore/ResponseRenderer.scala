package net.ceedubs.scrutinator.petstore

import scalaz._
import org.scalatra._
import org.scalatra.validation.ValidationError

trait ResponseRenderer[A] {
  def apply(a: A): ActionResult
}

object ResponseRenderer {
  def renderer[A](f: A => ActionResult): ResponseRenderer[A] = new ResponseRenderer[A] {
    def apply(a: A) = f(a)
  }

  implicit val unitRenderer: ResponseRenderer[Unit] = renderer[Unit](_ => NoContent())

  implicit val stringRenderer: ResponseRenderer[String] = renderer[String](s => Ok(s))

  implicit val errorListRenderer: ResponseRenderer[NonEmptyList[ValidationError]] = {
    def showValidationError(error: ValidationError): String = {
      val fieldNameDescription = error.field.map(f => s" (${f.name})")
      s"${error.message}${fieldNameDescription.getOrElse("")}"
    }

    renderer(errors => UnprocessableEntity(errors.list.map(e => showValidationError(e)).mkString(", ")))
  }

  implicit def optionRenderer[A](implicit rendererA: ResponseRenderer[A]): ResponseRenderer[Option[A]] =
    renderer(_.fold(NotFound())(rendererA.apply))

  implicit def errorsOrResultRenderer[A](implicit rendererA: ResponseRenderer[A]): ResponseRenderer[NonEmptyList[ValidationError] \/ A] =
    renderer(_.fold(errorListRenderer.apply, rendererA.apply))
}
