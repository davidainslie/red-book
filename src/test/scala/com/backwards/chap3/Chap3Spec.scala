package com.backwards.chap3

import scala.annotation.tailrec
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Chap3Spec extends AnyWordSpec with Matchers {
  /**
   * `List` data type, parameterized on a type, `A`.
   * @tparam A The element type
   */
  sealed trait List[+A]

  /**
   * A `List` data constructor representing the empty list.
   */
  case object Nil extends List[Nothing]

  /**
   * Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.
   * @param head A Head element of non empty list
   * @param tail List[A]
   * @tparam A The element type
   */
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  /**
   * `List` companion object - Contains functions for creating and working with lists.
   */
  object List {
    def apply[A](as: A*): List[A] = // Variadic function syntax
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    /**
     * A function that uses pattern matching to add up a list of integers.
     * @param ints List[Int]
     * @return Int
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

    def tail[A](xs: List[A]): List[A] = xs match {
      case Nil => sys.error("Cannot 'tail' Nil")
      case Cons(_, t) => t
    }

    def setHead[A](xs: List[A], x: A): List[A] = xs match {
      case Nil => sys.error("Cannot 'setHead' on Nil")
      case Cons(_, t) => Cons(x, t)
    }

    @tailrec
    def drop[A](xs: List[A], n: Int): List[A] = xs match {
      case Nil => Nil
      case c @ Cons(_, t) => if (n > 0) drop(t, n - 1) else c
    }

    @tailrec
    def dropWhile[A](xs: List[A])(f: A => Boolean): List[A] = xs match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case l => l
    }

    def append[A](xs: List[A], ys: List[A]): List[A] = xs match {
      case Nil => ys
      case Cons(h, t) => Cons(h, append(t, ys))
    }

    def init[A](xs: List[A]): List[A] = xs match {
      case Nil => sys.error("Cannot 'init' Nil")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

    def `init better`[A](xs: List[A]): List[A] = {
      @tailrec
      def go(xs: List[A], acc: List[A]): List[A] = xs match {
        case Nil => sys.error("Cannot 'init' Nil")
        case Cons(_, Nil) => acc
        case Cons(h, t) => go(t, append(acc, List(h)))
      }

      go(xs, Nil)
    }
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

  "3.2 tail - for removing first element of a list" in {
    import List._

    val xs = List(1, 2, 3, 4, 5)

    tail(xs) mustBe List(2, 3, 4, 5)
    an [Exception] must be thrownBy tail(Nil)
  }

  "3.3 setHead" in {
    import List._

    val xs = List(1, 2, 3, 4, 5)

    setHead(xs, 99) mustBe List(99, 2, 3, 4, 5)
    an [Exception] must be thrownBy setHead(Nil, 99)
  }

  "3.4 drop - to generalize tail" in {
    import List._

    val xs = List(1, 2, 3, 4, 5)

    drop(xs, 3) mustBe List(4, 5)
    drop(Nil, 1) mustBe Nil
  }

  "3.5 dropWhile" in {
    import List._

    val xs = List(1, 2, 3, 4, 5)

    val lessThan3: Int => Boolean =
      _ < 3

    dropWhile(xs)(lessThan3) mustBe List(3, 4, 5)
    dropWhile(Nil)(lessThan3) mustBe Nil
  }

  "3.6 init" in {
    import List._

    val xs = List(1, 2, 3, 4, 5)

    init(xs) mustBe List(1, 2, 3, 4)
    an [Exception] must be thrownBy init(Nil)

    `init better`(xs) mustBe List(1, 2, 3, 4)
    an [Exception] must be thrownBy `init better`(Nil)
  }
}