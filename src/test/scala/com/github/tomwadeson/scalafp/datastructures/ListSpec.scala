package com.github.tomwadeson.scalafp.datastructures

import com.github.tomwadeson.scalafp.datastructures.List._
import org.scalatest.{FlatSpec, Matchers}

class ListSpec extends FlatSpec with Matchers {

  "List" should "define foldLeft" in {
    val xs = List(1,2,3)
    val sumOfList = xs.foldLeft(0)(_ + _)
    sumOfList should be(6)
  }

  it should "define foldRight" in {
    val xs = List(1,2,3)
    val sumOfList = xs.foldRight(0)(_ + _)
    sumOfList should be(6)
  }

  it should "define reverse" in {
    List(1,2,3).reverse should be(List(3,2,1))
  }

  it should "define append" in {
    List(1,2,3) ++ List(4,5,6) should be(List(1,2,3,4,5,6))
  }

  it should "provide a Monoid instance" in {
    import com.github.tomwadeson.scalafp.typeclasses.Monoid.ops._

    val xs = List(1,2)
    val ys = List(3,4)

    xs |+| ys should be (List(1,2,3,4))
    xs |+| Nil should be(xs)
    List.empty[Int] |+| xs should be (xs)
  }

  it should "provide a Functor instance" in {
    import com.github.tomwadeson.scalafp.typeclasses.Functor.ops._

    val xs = List(1,2,3)
    xs.map(_ * 2) should be(List(2,4,6))
    List.empty[Int].map(_ * 2) should be(Nil)
  }
}
