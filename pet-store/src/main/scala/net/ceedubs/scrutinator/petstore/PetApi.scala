package net.ceedubs.scrutinator.petstore

import org.scalatra._
import org.scalatra.swagger.Swagger
import scalate.ScalateSupport
import net.ceedubs.scrutinator._
import net.ceedubs.scrutinator.scalatra._
import net.ceedubs.scrutinator.swagger._
import net.ceedubs.scrutinator.json4s._
import shapeless._
import shapeless.record._
import shapeless.syntax.singleton._
import shapeless.ops.record._
import scalaz._

object PetApi {
  object Models {
    val category = ModelWithId(
      id = "Category",
      model = Model(
        ('id ->> Field[Int]()) ::
        ('name ->> Field[String]()) ::
        HNil))

    val tag = ModelWithId(
      id = "Tag",
      model = Model(
        ('id ->> Field[Int]()) ::
        ('name ->> Field[String]()) ::
        HNil))

    val pet = ModelWithId(
      id = "Pet",
      model = Model(
        ('id ->> Field[Int](
          description = Some("unique identifier for the pet")
        ).required()) ::
        ('category ->> ModelField(category)) ::
        ('name ->> Field[String]().required()) ::
        ('photoUrls ->> Field[List[String]]()) ::
        ('tags ->> CollectionField[List].ofModel(tag)) ::
        HNil))
  }

  val categoryGen = LabelledGeneric[Category]

  val tagGen = LabelledGeneric[Tag]

  val petGen = LabelledGeneric[Pet]

  val petBody =
    ('body ->> JsonParam(ModelField(model = Models.pet))) ::
    HNil

  val petBodyValidator = ScalatraSupport.validator(petBody).map(params =>
    petGen.from(params.get('body).
    updateWith('category)(_.map(categoryGen.from)).
    updateWith('tags)(_.map(_.map(tagGen.from)))))

  implicit val petRenderer: ResponseRenderer[Pet] = ResponseRenderer.renderer[Pet](p => Ok(p.toString))
}

class PetApi(petDao: PetDao)(implicit swagger: Swagger) extends PetStoreServlet {
  import PetApi._

  override protected val applicationName = Some("pets")
  override protected val applicationDescription = "Operations about pets"

  val addPetFields =
    ('body ->> JsonParam(ModelField(
      model = Models.pet,
      description = Some("Pet object that needs to be added to the store")))) ::
    HNil

  val addPetOp = apiOperation[Unit]("addPet").
    summary("Add a new pet to the store").
    withParams(addPetFields)

  val addPet = renderer(petBodyValidator.map(petDao.save))

  post("/", operation(addPetOp))(addPet.run(request))

  val getPetByIdFields =
    ('petId ->> PathParam(Field[Int](
      description = Some("ID of pet that needs to be fetched")).
      required(_ => "A pet ID is required"))) :: HNil

  val getPetByIdOp = apiOperation[Pet]("getPetById").
    summary("Find pet by ID").
    notes("Returns a pet based on ID").
    withParams(getPetByIdFields)


  val getPetById = renderer(validator(getPetByIdFields).map(params =>
    petDao.byId(params.get('petId))))

  get("/:petId", operation(getPetByIdOp))(getPetById.run(request))

}
