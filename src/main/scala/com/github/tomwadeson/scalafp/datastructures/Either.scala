package com.github.tomwadeson.scalafp.datastructures

import Either.{Left, Right}

sealed trait Either[+E, +A] {
  def isLeft: Boolean = this match {
    case Left(_) => true
    case Right(_) => false
  }

  def isRight: Boolean = !isLeft

  def fold[T](l: E => T)(r: A => T): T = this match {
    case Left(x) => l(x)
    case Right(x) => r(x)
  }
}

object Either {
  final case class Left[+E](value: E) extends Either[E, Nothing]
  final case class Right[+A](value: A) extends Either[Nothing, A]
}
