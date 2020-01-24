package com.backwards.chap3

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Chap3Spec extends AnyWordSpec with Matchers {
  /**
   * `List` data type, parameterized on a type, `A`.
   * @tparam A
   */
  sealed trait List[+A]

  /**
   * A `List` data constructor representing the empty list.
   */
  case object Nil extends List[Nothing]

  /**
   * Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.
   * @param head A
   * @param tail List[A]
   * @tparam A
   */
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  /**
   * `List` companion object - Contains functions for creating and working with lists.
   */
  object List {
    /**
     * A function that uses pattern matching to add up a list of integers.
     * @param ints
     * @return
     */
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] = // Variadic function syntax
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  "3.1 What will be the result of the following match expression?" in {
    import List._

    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    x mustBe 3
  }
}