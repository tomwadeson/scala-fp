package com.github.tomwadeson.scalafp.datastructures

import com.github.tomwadeson.scalafp.datastructures.List._
import org.scalatest.{FlatSpec, Matchers}

class ListSpec extends FlatSpec with Matchers {

  "List" should "provide a Monoid instance" in {
    import com.github.tomwadeson.scalafp.typeclasses.Monoid.ops._

    val xs: List[Int] = Cons(1, Cons(2, Nil))
    val ys: List[Int] = Cons(3, Cons(4, Nil))

    xs |+| ys should be (Cons(1, Cons(2, Cons(3, Cons(4, Nil)))))
    xs |+| Nil should be(xs)
    (Nil: List[Int]) |+| xs should be (xs)
  }

  it should "provide a Functor instance" in {
    import com.github.tomwadeson.scalafp.typeclasses.Functor.ops._

    val xs: List[Int] = Cons(1, Cons(2, Cons(3, Nil)))
    xs.map(_ * 2) should be(Cons(2, Cons(4, Cons(6, Nil))))
    (Nil: List[Int]).map(_ * 2) should be(Nil)
  }
}
