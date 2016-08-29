package com.github.tomwadeson.scalafp.datastructures

import com.github.tomwadeson.scalafp.typeclasses.{Functor, Monoid}

sealed trait List[+A] {
  def foldLeft[B](acc: B)(f: (B, A) => B): B
  def foldRight[B](acc: B)(f: (A, B) => B): B
  def reverse: List[A]
  def ++[B >: A](other: List[B]): List[B]
}

object List {

  final case class Cons[+A](head: A, tail: List[A]) extends List[A] {

    def foldLeft[B](acc: B)(f: (B, A) => B): B = this match {
      case (Cons(h, t)) => t.foldLeft(f(acc, h))(f)
    }

    def foldRight[B](acc: B)(f: (A, B) => B): B =
      this.reverse.foldLeft(acc)((x, y) => f(y, x))

    def reverse: List[A] =
      this.foldLeft(empty[A])((x, y) => Cons(y, x))

    def ++[B >: A](other: List[B]): List[B] =
      this.foldRight(other)(Cons(_, _))
  }

  case object Nil extends List[Nothing] {
    def foldLeft[B](acc: B)(f: (B, Nothing) => B): B =
      acc

    def foldRight[B](acc: B)(f: (Nothing, B) => B): B =
      acc

    def reverse: List[Nothing] =
      this

    def ++[B >: Nothing](other: List[B]): List[B] =
      other
  }

  def apply[A](elems: A*): List[A] =
    elems.foldRight(empty[A])(Cons(_, _))

  def empty[A]: List[A] =
    Nil

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