package com.github.tomwadeson.scalafp.datastructures

import Either.{Left, Right}

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
}
