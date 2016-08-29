package com.github.tomwadeson.scalafp.typeclasses

import simulacrum.typeclass

@typeclass trait MonadPlus[F[_]] extends Monad[F] {
  def empty[A]: F[A]
  def filter[A](fa: F[A])(p: A => Boolean): F[A]
}
