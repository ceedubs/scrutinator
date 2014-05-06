package net.ceedubs.scrutinator.petstore

case class Category(
  id: Option[Int],
  name: Option[String])

case class Tag(
  id: Option[Int],
  name: Option[String])

case class Pet(
  id: Int,
  category: Option[Category],
  name: String,
  photoUrls: Option[List[String]],
  tags: Option[List[Tag]])
