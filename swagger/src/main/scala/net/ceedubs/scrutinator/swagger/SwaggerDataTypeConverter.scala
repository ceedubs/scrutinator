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

  implicit def wrappedSwaggerDataConverter[C[_], A](implicit wrapper: SwaggerWrappedDataTypeConverter[C], converter: SwaggerCoreDataTypeConverter[A]): SwaggerCoreDataTypeConverter[C[A]] =
    apply[C[A]](wrapper(converter.dataType))
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
