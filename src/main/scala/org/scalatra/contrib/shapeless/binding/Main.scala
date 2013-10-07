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

    val output1 = bindAllFromRequest(fields) map Function.tupled { (age, name, visitCount) =>
      val ageDesc = age.map(_ + " years old").getOrElse("an unkown age")
      s"$name is $ageDesc and has visited $visitCount times"
    }

    // output1 is a Validation[(Option[Int], String, Int)]
    println(output1)

    // OR

    val output2 = for {
      validParams <- bindAllFromRequest(fields)
    } yield {
      val (age, name, visitCount) = validParams
      val ageDesc = age.map(_ + " years old").getOrElse("an unkown age")
      s"$name is $ageDesc and has visited $visitCount times"
    }

    // output2 is a Validation[(Option[Int], String, Int)]
    println(output2)
  }
}
