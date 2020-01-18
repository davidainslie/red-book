package com.backwards.chap2

import scala.annotation.tailrec
import scala.reflect.ClassTag
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Chap2Spec extends AnyWordSpec with Matchers {
  "2.1 Fibonacci" in {
    def fib(n: Int): Int = {
      @tailrec
      def fib(x: Int, a: Int, b: Int): Int =
        if (x == 0) a
        else fib(x - 1, b, a + b)

      fib(n, 0, 1)
    }

    fib(0) mustBe 0
    fib(1) mustBe 1
    fib(2) mustBe 1
    fib(5) mustBe 5
    fib(8) mustBe 21
    fib(12) mustBe 144
  }

  "2.2 Sorted" in {
    @tailrec
    def isSorted[A: ClassTag](as: Array[A], ordered: (A, A) => Boolean): Boolean = as match {
      case as if as.length <= 1 =>
        true

      case Array(a1, a2, rest @ _*) =>
        if (ordered(a1, a2))
          isSorted(Array(a2) ++  Array(rest: _*), ordered)
        else
          false
    }

    isSorted[Int](Array(1, 2, 3), _ <= _) mustBe true
    isSorted[Int](Array(1, 5, 3), _ <= _) mustBe false

    isSorted[String](Array("a", "b", "c"), _ <= _) mustBe true
    isSorted[String](Array("a", "z", "c"), _ <= _) mustBe false
  }

  "2.3 Curry" in {
    def curry[A, B, C](f: (A, B) => C): A => B => C =
      a => b => f(a, b)

    def f(a: Int, b: String): Boolean =
      a.toString == b

    curry(f)(42)("hi") mustBe false
    curry(f)(42)("42") mustBe true
  }

  "2.4 Uncurry" in {
    def uncurry[A, B, C](f: A => B => C): (A, B) => C =
      (a, b) => f(a)(b)

    def f(a: Int)(b: String): Boolean =
      a.toString == b

    uncurry(f)(42, "hi") mustBe false
    uncurry(f)(42, "42") mustBe true
  }

  "2.5 Compose" in {
    def compose[A, B, C](f: B => C, g: A => B): A => C =
      a => f(g(a))

    def f(b: Int): String = b.toString

    def g(a: Boolean): Int = if (a) 1 else -1

    compose(f, g)(true) mustBe "1"
    compose(f, g)(false) mustBe "-1"
  }
}