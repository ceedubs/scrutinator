import net.ceedubs.scrutinator.petstore._

import org.scalatra._
import javax.servlet.ServletContext

class ScalatraBootstrap extends LifeCycle {
  override def init(context: ServletContext) {
    implicit val swagger = new PetStoreSwagger
    context.mount(new PetApi(MapPetDao.dummyInstance), "/pets")
    context.mount(new ResourcesApp, "/api-docs")
  }
}
