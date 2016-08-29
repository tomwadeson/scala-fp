package com.github.tomwadeson.scalafp.datastructures

sealed trait Option[+A]

object Option {
  final case class Some[+A](value: A) extends Option[A]
  case object None extends Option[Nothing]
}