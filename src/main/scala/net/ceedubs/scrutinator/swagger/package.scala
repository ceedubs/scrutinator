package net.ceedubs.scrutinator

import swagger.SwaggerShowInstances

import scalaz._

package object swagger extends SwaggerShowInstances {
  type SwaggerShow[A] = Show[A @@ SwaggerSpec]
}
