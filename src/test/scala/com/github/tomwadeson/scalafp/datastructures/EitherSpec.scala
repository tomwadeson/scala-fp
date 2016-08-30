package com.github.tomwadeson.scalafp.datastructures

import com.github.tomwadeson.scalafp.datastructures.Either.{Left, Right}
import com.github.tomwadeson.scalafp.typeclasses.Applicative
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

  it should "define a Functor instance" in {
    import com.github.tomwadeson.scalafp.typeclasses.Functor.ops._

    val f: Int => Int = (_ + 10)

    x.map(f) should be(x)
    y.map(f) should be(Right(30))
  }

  it should "define an Applicative instance" in {
    import com.github.tomwadeson.scalafp.typeclasses.Applicative.ops._

    val x: Either[String, Int] = Right(10)
    val f: Either[String, Int => Int] = Right(_ * 10)

    x <*> f should be(Right(100))
  }

  it should "define a Monad instance" in {
    import com.github.tomwadeson.scalafp.typeclasses.Monad.ops._

    val x: Either[String, Int] = Right(10)
    val y: Either[String, Int] = Right(0)
    val f: Int => Either[String, String] =
      x => if (x > 0) Right("Yes!") else Left("No!")

    (x >>= f) should be(Right("Yes!"))
    (y >>= f) should be(Left("No!"))
  }
}
