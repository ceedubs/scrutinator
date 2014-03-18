package net.ceedubs.scrutinator
package swagger

import scalaz._
import shapeless._

sealed trait SwaggerSpec

object SwaggerSpec {
  def apply[A](a: A): A @@ SwaggerSpec = Tag[A, SwaggerSpec](a)
}

object SwaggerShow {
  sealed trait SwaggerSpec

  def show[A](s: Show[A]): SwaggerShow[A] = Tag.subst(s)
}

trait SwaggerShowInstances {
  import SwaggerShow._

  implicit val swaggerShowString: SwaggerShow[String] = SwaggerShow.show(scalaz.std.string.stringInstance)
  implicit val swaggerShowBoolean: SwaggerShow[Boolean] = SwaggerShow.show(scalaz.std.anyVal.booleanInstance)
  implicit val swaggerShowByte: SwaggerShow[Byte] = SwaggerShow.show(scalaz.std.anyVal.byteInstance)
  implicit val swaggerShowChar: SwaggerShow[Char] = SwaggerShow.show(scalaz.std.anyVal.char)
  implicit val swaggerShowDouble: SwaggerShow[Double] = SwaggerShow.show(scalaz.std.anyVal.doubleInstance)
  implicit val swaggerShowFloat: SwaggerShow[Float] = SwaggerShow.show(scalaz.std.anyVal.floatInstance)
  implicit val swaggerShowInt: SwaggerShow[Int] = SwaggerShow.show(scalaz.std.anyVal.intInstance)
  implicit val swaggerShowLong: SwaggerShow[Long] = SwaggerShow.show(scalaz.std.anyVal.longInstance)
  implicit val swaggerShowShort: SwaggerShow[Short] = SwaggerShow.show(scalaz.std.anyVal.shortInstance)
}
