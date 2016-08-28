package com.github.tomwadeson.scalafp.typeclasses

import simulacrum.typeclass

@typeclass trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
