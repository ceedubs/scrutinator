package net.ceedubs.scrutinator
package swagger

import org.scalatra.swagger.DataType

trait SwaggerCoreDataTypeConverter[A] {
  def dataType: DataType
}

object SwaggerCoreDataTypeConverter {
  def converter[A](d: DataType): SwaggerCoreDataTypeConverter[A] = new SwaggerCoreDataTypeConverter[A] {
    val dataType = d
  }

  implicit val intSwaggerDataConverter: SwaggerCoreDataTypeConverter[Int] = converter[Int](DataType.Int)

  implicit val stringSwaggerDataConverter: SwaggerCoreDataTypeConverter[String] = converter[String](DataType.String)

  implicit val booleanSwaggerDataConverter: SwaggerCoreDataTypeConverter[Boolean] = converter[Boolean](DataType.Boolean)

  implicit val longSwaggerDataConverter: SwaggerCoreDataTypeConverter[Long] = converter[Long](DataType.Long)

  implicit val floatSwaggerDataConverter: SwaggerCoreDataTypeConverter[Float] = converter[Float](DataType.Float)

  implicit val doubleSwaggerDataConverter: SwaggerCoreDataTypeConverter[Double] = converter[Double](DataType.Double)

  implicit val dateSwaggerDataConverter: SwaggerCoreDataTypeConverter[java.util.Date] = converter[java.util.Date](DataType.Date)

  implicit def wrappedSwaggerDataConverter[C[_], A](implicit wrapper: SwaggerWrappedDataTypeConverter[C], c: SwaggerCoreDataTypeConverter[A]): SwaggerCoreDataTypeConverter[C[A]] =
    converter[C[A]](wrapper(c.dataType))
}

trait SwaggerWrappedDataTypeConverter[C[_]] {
  def apply(d: DataType): DataType
}

object SwaggerWrappedDataTypeConverter {
  def wrapper[C[_]](f: DataType => DataType): SwaggerWrappedDataTypeConverter[C] = new SwaggerWrappedDataTypeConverter[C] {
    def apply(d: DataType) = f(d)
  }

  implicit val listDataTypeConverter: SwaggerWrappedDataTypeConverter[List] = wrapper[List](DataType.GenList(_))

  implicit val setDataTypeConverter: SwaggerWrappedDataTypeConverter[Set] = wrapper[Set](DataType.GenSet(_))

  implicit val vectorDataTypeConverter: SwaggerWrappedDataTypeConverter[Vector] = wrapper[Vector](DataType.GenList(_))

  implicit val seqDataTypeConverter: SwaggerWrappedDataTypeConverter[Seq] = wrapper[Seq](DataType.GenList(_))

  implicit val indexedSeqDataTypeConverter: SwaggerWrappedDataTypeConverter[IndexedSeq] = wrapper[IndexedSeq](DataType.GenList(_))
}
