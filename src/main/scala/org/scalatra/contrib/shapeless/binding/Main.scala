package org.scalatra.contrib.shapeless.binding

import RequestBindingSupport.bindAllFromRequest
import org.scalatra.validation.ValidationError

object Main {
  def main(args: Array[String]) {

    implicit val request: Request = new Request {
      val queryParams = Map(
//        "age" -> 20.toString,
//        "age" -> "26s",
        "name" -> "Cody"
      )

      override def toString = "Mock request " + super.toString
    }

    // requiredParam[Int]("name", Header)
    // optionalParam[Int]("name", Query)
    // paramWithDefault[Int]("name", Body, 7)

    // paramWithDefault[Int]("name", Body, 7).description("blah").validating(x => test)

    val fields = (
      new RequestBinding { self =>
        type Value = Int
        type Output = Option[Int]

        val field = new RequestField {
          val required: Boolean = false

          val key: RequestKey = new RequestKey {
            val source = ValueSource.QueryString

            val name: String = "age"

            type Source = ValueSource.QueryString
          }

          val validations: List[Int => Option[ValidationError]] = Nil

          val description: Option[String] = Some("the age")

          type Value = self.Value
        }
        val reader = QueryParamReader.intQueryParamReader
        val validator = new OptionalValueBoundValueValidator {
          def requiredError: ValidationError = ValidationError("required")

          type Value = self.Value
        }
      }: RequestBinding.Aux[Int, Option[Int]],

      new RequestBinding { self =>
        type Value = String
        type Output = String

        val field = new RequestField {
          val required: Boolean = true

          val key: RequestKey = new RequestKey {
            val source = ValueSource.QueryString

            val name: String = "name"

            type Source = ValueSource.QueryString
          }

          val validations: List[String => Option[ValidationError]] = Nil

          val description: Option[String] = Some("the name")

          type Value = self.Value
        }
        val reader = QueryParamReader.stringQueryParamReader
        val validator = new RequiredValueBoundValueValidator {
          def requiredError: ValidationError = ValidationError("required")

          type Value = self.Value
        }
      }: RequestBinding.Aux[String, String]
    )

//    val fields = (
//      queryParam[Int]("age").validating(
//        age => age.filter(_ > 21).map(_ => Nil).getOrElse(List(ValidationError("Age must be greater than 21")))),
//      queryParam[String]("name").required)

    val output = for {
      validParams <- bindAllFromRequest(fields)
    } yield {
      val (age, name) = validParams
      val ageDesc = age.map(_ + " years old").getOrElse("an unkown age")
      s"$name is $ageDesc"
    }

    println(output)
  }
}
