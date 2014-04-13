package net.ceedubs.scrutinator

import swagger.SwaggerShowInstances

import scalaz.{ @@ => _, _ }
import shapeless.tag
import shapeless.tag._
import org.scalatra.swagger.{ Model => SwaggerModel }

package object swagger extends SwaggerShowInstances {
  type SwaggerShow[A] = Show[A @@ SwaggerSpec]
  type ModelId = String @@ ModelIdTag
  type ModelsCache = Map[ModelId, SwaggerModel]
  type ModelState[A] = State[ModelsCache, A]
}
