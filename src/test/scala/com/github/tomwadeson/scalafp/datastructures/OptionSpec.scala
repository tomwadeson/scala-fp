package com.github.tomwadeson.scalafp.datastructures

import com.github.tomwadeson.scalafp.datastructures.Option.{None, Some}
import org.scalatest.{FlatSpec, Matchers}

class OptionSpec extends FlatSpec with Matchers {

  "Option" should "define getOrElse" in {
    val x: Option[Int] = Some(1)
    val y: Option[Int] = None

    x.getOrElse(42) should be(1)
    y.getOrElse(42) should be(42)
  }

}
