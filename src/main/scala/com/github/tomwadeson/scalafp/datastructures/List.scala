package com.github.tomwadeson.scalafp.datastructures

sealed trait List[+A]

object List {
  final case class Cons[+A](head: A, tail: List[A]) extends List[A]
  case object Nil extends List[Nothing]
}
