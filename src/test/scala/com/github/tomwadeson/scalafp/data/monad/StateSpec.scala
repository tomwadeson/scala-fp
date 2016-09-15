package com.github.tomwadeson.scalafp.data.monad

import com.github.tomwadeson.scalafp.data.List
import com.github.tomwadeson.scalafp.data.List.Cons
import org.scalatest.{FlatSpec, Matchers}

class StateSpec extends FlatSpec with Matchers {

  "State" should "thread state when binding" in {
    val s = Stack.pushPushPop(100, 200)
    s.evalState(List()) should be(200)
    s.execState(List()) should be(List(100))
  }
}

object Stack {
  import com.github.tomwadeson.scalafp.typeclasses.Monad.ops._

  type Stack = List[Int]

  def pop: State[Stack, Int] = State[Stack, Int] {
    case Cons(x, xs) => (x, xs)
  }

  def push(a: Int): State[Stack, Unit] = State[Stack, Unit] {
    case xs => ((), Cons(a, xs))
  }

  def pushPushPop(a1: Int, a2: Int): State[Stack, Int] = for {
    _ <- push(a1)
    _ <- push(a2)
    x <- pop
  } yield x
}
