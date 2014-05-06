package net.ceedubs.scrutinator.petstore

trait PetDao {
  def save(pet: Pet): Unit

  def byId(id: Int): Option[Pet]
}

class MapPetDao(initialPets: Map[Int, Pet]) extends PetDao {
  private[this] val pets = collection.mutable.HashMap(initialPets.toSeq: _*)

  def save(pet: Pet) = pets.put(pet.id, pet)

  def byId(id: Int) = pets.get(id)
}

object MapPetDao {
  val dummyInstance: PetDao = {
    val pets = Map(
      1 -> Pet(
        id = 1,
        category = Some(Category(
          id = Some(11),
          name = Some("category 11"))),
        name = "fido",
        photoUrls = None,
        tags = Some(List(
          Tag(
            id = Some(21),
            name = Some("tag 21")),
          Tag(
            id = Some(22),
            name = Some("tag 22"))))))

    new MapPetDao(pets)
  }
}
