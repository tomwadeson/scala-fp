package com.github.tomwadeson.scalafp.datastructures

import com.github.tomwadeson.scalafp.typeclasses.{Functor, Monoid}

sealed trait List[+A]

object List {

  final case class Cons[+A](head: A, tail: List[A]) extends List[A]

  case object Nil extends List[Nothing]

  // Typeclass instances

  implicit def MonoidInstance[A] = new Monoid[List[A]] {
    def id: List[A] = Nil

    def append(x: List[A], y: List[A]): List[A] = (x, y) match {
      case (Nil, Nil) => Nil
      case (Nil, y) => y
      case (x, Nil) => x
      case (Cons(h, t), y) => Cons(h, append(t, y))
    }
  }

  implicit val FunctorInstance = new Functor[List] {
    def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }
  }
}