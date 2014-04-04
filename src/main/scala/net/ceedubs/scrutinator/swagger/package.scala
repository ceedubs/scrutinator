package net.ceedubs.scrutinator

import swagger.SwaggerShowInstances

import scalaz._
import org.scalatra.swagger.Model

package object swagger extends SwaggerShowInstances {
  type SwaggerShow[A] = Show[A @@ SwaggerSpec]
  type ModelId = String @@ SwaggerModelId
  type ModelsCache = Map[ModelId, Model]
  type ModelState[A] = State[ModelsCache, A]
}
