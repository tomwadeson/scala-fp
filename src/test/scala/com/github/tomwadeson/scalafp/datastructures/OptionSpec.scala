package com.github.tomwadeson.scalafp.datastructures

import com.github.tomwadeson.scalafp.datastructures.Option.{None, Some}
import com.github.tomwadeson.scalafp.typeclasses.Applicative
import org.scalatest.{FlatSpec, Matchers}

class OptionSpec extends FlatSpec with Matchers {

  val w: Option[Int] = Some(-1)
  val x: Option[Int] = Some(1)
  val y: Option[Int] = None

  "Option" should "define getOrElse" in {
    x.getOrElse(42) should be(1)
    y.getOrElse(42) should be(42)
  }

  it should "provide a Functor instance" in {
    import com.github.tomwadeson.scalafp.typeclasses.Functor.ops._

    x.map(_ + 1) should be(Some(2))
    y.map(_ + 1) should be(None)
  }

  it should "provide an Applicative instance" in {
    import com.github.tomwadeson.scalafp.typeclasses.Applicative.ops._

    val f: Option[Int => Int] = Applicative[Option].pure(_ + 10)

    x <*> f should be(Some(11))
    y <*> f should be(None)
  }

  it should "provide a Monad instance" in {
    import com.github.tomwadeson.scalafp.typeclasses.Monad.ops._

    val f: Int => Option[Boolean] = (x => if (x > 0) Some(true) else Some(false))
    (w >>= f) should be(Some(false))
    (x >>= f) should be(Some(true))
    (y >>= f) should be(None)
  }

  it should "provide a MonadPlus instance" in {
    import com.github.tomwadeson.scalafp.typeclasses.MonadPlus.ops._

    val p: Int => Boolean = (_ > 0)
    w.filter(p) should be(None)
    x.filter(p) should be(x)
    y.filter(p) should be(None)
  }

  it should "support for-comprehensions" in {
    import com.github.tomwadeson.scalafp.typeclasses.MonadPlus.ops._

    val expr1 = for {
      w <- w
      if w == -1
      x <- x
    } yield x

    expr1 should be(Some(1))

    val expr2 = for {
      y <- y
      z <- expr1
    } yield z

    expr2 should be(None)
  }
}
