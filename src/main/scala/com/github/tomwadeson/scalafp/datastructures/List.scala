package com.github.tomwadeson.scalafp.datastructures

import com.github.tomwadeson.scalafp.typeclasses.{Applicative, Functor, Monad, Monoid}

sealed trait List[+A] {
  def foldLeft[B](acc: B)(f: (B, A) => B): B

  def foldRight[B](acc: B)(f: (A, B) => B): B

  def reverse: List[A]

  def ++[B >: A](other: List[B]): List[B]

  def map[B](f: A => B): List[B]
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

    def map[B](f: (A) => B): List[B] =
      Functor[List].map(this)(f)
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

    def map[B](f: (Nothing) => B): List[B] =
      empty[B]
  }

  def apply[A](elems: A*): List[A] =
    elems.foldRight(empty[A])(Cons(_, _))

  def empty[A]: List[A] =
    Nil

  // Typeclass instances

  implicit def MonoidInstance[A] = new Monoid[List[A]] {
    def id: List[A] = Nil

    def append(x: List[A], y: List[A]): List[A] =
      x ++ y
  }

  implicit val FunctorInstance = new Functor[List] {
    def map[A, B](fa: List[A])(f: (A) => B): List[B] =
      fa.foldRight(empty[B])((x, acc) => Cons(f(x), acc))
  }

  implicit val ApplicativeInstance = new Applicative[List] {
    def pure[A](a: A): List[A] =
      List(a)

    def ap[A, B](fa: List[A])(f: List[(A) => B]): List[B] = {
      // Annoying that I can't just use `map` here, but there's a cyclic dependency (definition is overridden in `Applicative`)
      val functor = implicitly[Functor[List]]
      val nested = functor.map(f)(g => functor.map(fa)(x => g(x)))
      nested.foldRight(empty[B])(_ ++ _)
    }
  }

  implicit val MonadInstance = new Monad[List] {
    def flatMap[A, B](fa: List[A])(f: (A) => List[B]): List[B] = {
      val functor = implicitly[Functor[List]]
      val nested = functor.map(fa)(f)
      nested.foldRight(empty[B])(_ ++ _)
    }

    def pure[A](a: A): List[A] =
      List(a)
  }
}