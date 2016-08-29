package com.github.tomwadeson.scalafp.typeclasses

import simulacrum.{op, typeclass}

@typeclass trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]

  @op("<*>") def ap[A, B](fa: F[A])(f: F[A => B]): F[B]

  override def map[A, B](fa: F[A])(f: (A) => B): F[B] =
    ap(fa)(pure(f))
}
