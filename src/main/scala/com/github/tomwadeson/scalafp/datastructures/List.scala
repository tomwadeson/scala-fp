package com.github.tomwadeson.scalafp.datastructures

import com.github.tomwadeson.scalafp.datastructures.Option.{None, Some}
import com.github.tomwadeson.scalafp.typeclasses.{MonadPlus, Monoid}

sealed trait List[+A] {
  def foldLeft[B](acc: B)(f: (B, A) => B): B

  def foldRight[B](acc: B)(f: (A, B) => B): B

  def reverse: List[A]

  def ++[B >: A](other: List[B]): List[B]

  def safeHead: Option[A]

  def safeTail: Option[List[A]]
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

    def safeHead: Option[A] =
      Some(head)

    def safeTail: Option[List[A]] =
      Some(tail)
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

    def safeHead: Option[Nothing] =
      None

    def safeTail: Option[List[Nothing]] =
      None
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

  implicit val MonadPlusInstance = new MonadPlus[List] {
    def flatMap[A, B](fa: List[A])(f: (A) => List[B]): List[B] =
      map(fa)(f).foldRight(empty[B])(_ ++ _)

    def pure[A](a: A): List[A] =
      List(a)

    override def map[A, B](fa: List[A])(f: (A) => B): List[B] =
      fa.foldRight(empty[B])((x, acc) => Cons(f(x), acc))

    def empty[A]: List[A] =
      List()

    def filter[A](fa: List[A])(p: (A) => Boolean): List[A] =
      flatMap(fa)(x => if (p(x)) List(x) else empty)
  }
}