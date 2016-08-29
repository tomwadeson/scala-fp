package com.github.tomwadeson.scalafp.datastructures

import com.github.tomwadeson.scalafp.typeclasses.MonadPlus

sealed trait Option[+A] {
  def getOrElse[B >: A](default: B): B =
    this.fold(default)(identity)

  def fold[B](none: B)(some: A => B): B
}

object Option {
  final case class Some[+A](value: A) extends Option[A] {
    def fold[B](none: B)(some: (A) => B): B =
      some(value)
  }

  case object None extends Option[Nothing] {
    def fold[B](none: B)(some: (Nothing) => B): B =
      none
  }

  def apply[A](value: A): Option[A] =
    if (value == null) None else Some(value)

  implicit val MonadPlusInstance = new MonadPlus[Option] {
    def empty[A]: Option[A] =
      None

    def filter[A](fa: Option[A])(p: (A) => Boolean): Option[A] =
      fa.fold(empty[A])(x => if (p(x)) Some(x) else empty[A])

    def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] =
      map(fa)(f).fold(empty[B])(identity)

    def pure[A](a: A): Option[A] =
      Some(a)

    override def map[A, B](fa: Option[A])(f: (A) => B): Option[B] =
      fa.fold(empty[B])(x => Some(f(x)))
  }
}