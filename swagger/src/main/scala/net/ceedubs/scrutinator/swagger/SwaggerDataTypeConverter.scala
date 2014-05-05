package net.ceedubs.scrutinator
package swagger

import org.scalatra.swagger.DataType

trait SwaggerCoreDataTypeConverter[A] {
  def dataType: DataType
}

object SwaggerCoreDataTypeConverter {
  def apply[A](d: DataType): SwaggerCoreDataTypeConverter[A] = new SwaggerCoreDataTypeConverter[A] {
    val dataType = d
  }

  implicit val intSwaggerDataConverter: SwaggerCoreDataTypeConverter[Int] = apply[Int](DataType.Int)

  implicit val stringSwaggerDataConverter: SwaggerCoreDataTypeConverter[String] = apply[String](DataType.String)

  implicit val booleanSwaggerDataConverter: SwaggerCoreDataTypeConverter[Boolean] = apply[Boolean](DataType.Boolean)

  implicit val longSwaggerDataConverter: SwaggerCoreDataTypeConverter[Long] = apply[Long](DataType.Long)

  implicit val floatSwaggerDataConverter: SwaggerCoreDataTypeConverter[Float] = apply[Float](DataType.Float)

  implicit val doubleSwaggerDataConverter: SwaggerCoreDataTypeConverter[Double] = apply[Double](DataType.Double)

  implicit val dateSwaggerDataConverter: SwaggerCoreDataTypeConverter[java.util.Date] = apply[java.util.Date](DataType.Date)

  implicit def listSwaggerDataConverter[A](implicit aConverter: SwaggerCoreDataTypeConverter[A]): SwaggerCoreDataTypeConverter[List[A]] = apply[List[A]](DataType.GenList(aConverter.dataType))

  implicit def setSwaggerDataConverter[A](implicit aConverter: SwaggerCoreDataTypeConverter[A]): SwaggerCoreDataTypeConverter[Set[A]] = apply[Set[A]](DataType.GenSet(aConverter.dataType))

  implicit def vectorSwaggerDataConverter[A](implicit aConverter: SwaggerCoreDataTypeConverter[A]): SwaggerCoreDataTypeConverter[Vector[A]] = apply[Vector[A]](DataType.GenList(aConverter.dataType))

  implicit def seqSwaggerDataConverter[A](implicit aConverter: SwaggerCoreDataTypeConverter[A]): SwaggerCoreDataTypeConverter[Seq[A]] = apply[Seq[A]](DataType.GenList(aConverter.dataType))

  implicit def indexedSeqSwaggerDataConverter[A](implicit aConverter: SwaggerCoreDataTypeConverter[A]): SwaggerCoreDataTypeConverter[IndexedSeq[A]] = apply[IndexedSeq[A]](DataType.GenList(aConverter.dataType))
}
