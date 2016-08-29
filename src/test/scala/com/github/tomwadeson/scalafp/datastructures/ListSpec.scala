package com.github.tomwadeson.scalafp.datastructures

import com.github.tomwadeson.scalafp.datastructures.List._
import com.github.tomwadeson.scalafp.datastructures.Option.{None, Some}
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

  it should "define safeHead" in {
    List(1,2).safeHead should be(Some(1))
    List().safeHead should be(None)
  }

  it should "define safeTail" in {
    List(1,2).safeTail should be(Some(List(2)))
    List().safeTail should be(None)
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

  it should "provide an Applicative instance" in {
    import com.github.tomwadeson.scalafp.typeclasses.Applicative.ops._
    val f1: Int => Int = (_ + 1)
    val f2: Int => Int = (_ * 2)
    val fs = List(f1, f2)

    val xs = List(10,20)

    xs <*> fs should be(List(11, 21, 20, 40))
  }

  it should "provide a Monad instance" in {
    import com.github.tomwadeson.scalafp.typeclasses.Monad.ops._
    val xs = List(1,2,3,4,5)
    val f: Int => List[Int] = (x => if (x < 3) List(x, x) else List())
    (xs >>= f) should be(List(1, 1, 2, 2))
  }

  it should "provide a MonadPlus instance" in {
    import com.github.tomwadeson.scalafp.typeclasses.MonadPlus.ops._
    val xs = List(1,2,3,4,5)
    xs.filter(_ <= 3) should be(List(1,2,3))
  }

  it should "support for-comprehensions" in {
    import com.github.tomwadeson.scalafp.typeclasses.MonadPlus.ops._
    val xs = List(1,2,3,4,5)
    val ys = List(10, 20, 30, 40, 50)

    val sums = for {
      x <- xs
      if (x > 3)
      y <- ys
    } yield x + y

    sums should be(List(14, 24, 34, 44, 54, 15, 25, 35, 45, 55))
  }
}
