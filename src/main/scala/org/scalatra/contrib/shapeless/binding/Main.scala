package org.scalatra.contrib.shapeless.binding

import RequestBindingSupport._
import ValueSource._

object Main {
  def main(args: Array[String]) {

    implicit val request: Request = new Request {
      val queryParams = Map(
        "age" -> 26.toString,
//        "age" -> "26s",
        "name" -> "Cody"
      )

      override def toString = "Mock request " + super.toString
    }

    val fields = (
      optionalParam[Int, QueryString]("age"),
      requiredParam[String, QueryString]("name"),
      paramWithDefault[Int, QueryString]("visits", 0)
    )

    val output = for {
      validParams <- bindAllFromRequest(fields)
    } yield {
      val (age, name, visitCount) = validParams
      val ageDesc = age.map(_ + " years old").getOrElse("an unkown age")
      s"$name is $ageDesc and has visited $visitCount times"
    }

    println(output)
  }
}
