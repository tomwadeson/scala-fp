package com.github.tomwadeson.scalafp.datastructures

import com.github.tomwadeson.scalafp.datastructures.Either.{Left, Right}
import org.scalatest.{FlatSpec, Matchers}

class EitherSpec extends FlatSpec with Matchers {

  val x: Either[Int, Nothing] = Left(10)
  val y: Either[Nothing, Int] = Right(20)

  "Either" should "define isLeft and isRight" in {
    x.isLeft should be(true)
    x.isRight should be(false)
    y.isLeft should be(false)
    y.isRight should be(true)
  }

  it should "define fold" in {
    val f: Int => String = (_.toString)
    x.fold(f)(f) should be("10")
    y.fold(f)(f) should be("20")
  }
}
