package com.github.tomwadeson.scalafp.typeclasses

import simulacrum.{op, typeclass}

@typeclass trait Semigroup[A] {
  @op("|+|") def append(x: A, y: A): A
}
