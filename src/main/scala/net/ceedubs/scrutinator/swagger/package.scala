package net.ceedubs.scrutinator

import swagger.SwaggerShowInstances

import scalaz._
import org.scalatra.swagger.Model

package object swagger extends SwaggerShowInstances {
  type SwaggerShow[A] = Show[A @@ SwaggerSpec]
  type ModelsCache = Map[String, Model]
  type ModelState[A] = State[ModelsCache, A]
}
