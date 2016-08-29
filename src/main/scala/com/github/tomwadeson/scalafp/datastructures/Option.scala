package com.github.tomwadeson.scalafp.datastructures

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
}