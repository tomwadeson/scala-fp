package com.github.tomwadeson.scalafp.datastructures

import com.github.tomwadeson.scalafp.datastructures.Either.{Left, Right}
import com.github.tomwadeson.scalafp.typeclasses.Monad

sealed trait Either[+A, +B] {
  def isLeft: Boolean =
    fold(_ => true)(_ => false)

  def isRight: Boolean =
    fold(_ => false)(_ => true)

  def fold[T](l: A => T)(r: B => T): T = this match {
    case Left(x) => l(x)
    case Right(x) => r(x)
  }
}

object Either {
  final case class Left[+A](value: A) extends Either[A, Nothing]

  final case class Right[+B](value: B) extends Either[Nothing, B]

  implicit def MonadInstance[L] = new Monad[({type T[a] = Either[L, a]})#T] {
    def flatMap[A, B](fa: Either[L, A])(f: (A) => Either[L, B]): Either[L, B] =
      ???

    def pure[A](a: A): Either[L, A] =
      Right(a)

    override def map[A, B](fa: Either[L, A])(f: (A) => B): Either[L, B] =
      fa.fold(Left(_): Either[L, B])(x => Right(f(x)))
  }
}
