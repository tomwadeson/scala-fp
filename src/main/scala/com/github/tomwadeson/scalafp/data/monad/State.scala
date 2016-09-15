package com.github.tomwadeson.scalafp.data.monad

import com.github.tomwadeson.scalafp.typeclasses.Monad

case class State[S, A](runState: S => (A, S)) {
  def evalState(s: S): A =
    runState(s)._1

  def execState(s: S): S =
    runState(s)._2
}

object State {

  def put[S](newState: S): State[S, Unit] =
    State(s => ((), newState))

  def get[S]: State[S, S] =
    State(s => (s, s))

  implicit def MonadInstance[S] = new Monad[({type T[a] = State[S, a]})#T] {
    def flatMap[A, B](fa: State[S, A])(f: (A) => State[S, B]): State[S, B] =
      State (s => { val (v, ss) = fa.runState(s); f(v).runState(ss) } )

    def pure[A](a: A): State[S, A] =
      State(s => (a, s))
  }
}