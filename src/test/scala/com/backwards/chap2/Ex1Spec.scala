package com.backwards.chap2

import scala.annotation.tailrec
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Ex1Spec extends AnyWordSpec with Matchers {
  "Fibonacci" in {
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
}