package org.scalatra.contrib.shapeless.binding

import RequestBinding._
import scalaz.std.option._

object Main {
  def main(args: Array[String]) {

    implicit val request: Request = new Request {
      val queryParams = Map(
        "age" -> 25.toString,
//        "age" -> "26s",
        "name" -> "Cody"
      )

      override def toString = "Mock request " + super.toString
    }

    val fields = (queryParam[Int]("age"), queryParam[String]("name").required)

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
