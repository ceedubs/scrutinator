package net.ceedubs.scrutinator
package scalatra

import net.ceedubs.scrutinator.swagger._
import shapeless._
import org.scalatra.swagger.{ SwaggerSupport, SwaggerSupportSyntax }
import org.scalatra.swagger.SwaggerSupportSyntax.SwaggerOperationBuilder


trait ScrutinatorSwaggerSupport { self: SwaggerSupport =>

  implicit class ScrutinatorSwaggerOperationBuilder[T <: org.scalatra.swagger.SwaggerOperation](builder: SwaggerOperationBuilder[T]) {

    /** Adds parameters to the Swagger metadata for an operation and
     *  registers (side-effect) any models associated with those parameters
     */
    def withParams[L <: HList](fields: L, existingModels: ModelsCache = Map.empty)(implicit converter: FieldListSwaggerConverter[L]): SwaggerOperationBuilder[T] = {
      val (models, parameters) = converter(fields).apply(existingModels)
      models.values.foreach(registerModel)
      builder.parameters(parameters: _*)
    }

  }

}
