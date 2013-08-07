package org.scalatra.contrib.shapeless.binding

import RequestBinding._
import scalaz.std.option._

object Main {
  def main(args: Array[String]) {

    implicit val request: Request = new Request {
      val queryParams = Map(
        "age" -> 25.toString,
        "name" -> "Cody"
      )

      override def toString = "Mock request " + super.toString
    }

    val fields = (queryParam[Int]("age"), queryParam[String]("name"))

    val output = for {
      (age, name) <- bindAllFromRequest(fields)
    } yield {
      s"$name is $age years old"
    }

    println(output)
  }
}
