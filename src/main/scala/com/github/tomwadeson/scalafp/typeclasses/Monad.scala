package com.github.tomwadeson.scalafp.typeclasses

import simulacrum.{op, typeclass}

@typeclass trait Monad[F[_]] extends Applicative[F] {
  @op(">>=") def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def ap[A, B](fa: F[A])(f: F[(A) => B]): F[B] =
    flatMap(f)(map(fa))

  override def map[A, B](fa: F[A])(f: (A) => B): F[B] =
    flatMap(fa)(a => pure(f(a)))
}
