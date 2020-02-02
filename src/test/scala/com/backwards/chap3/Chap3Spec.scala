package com.backwards.chap3

import scala.annotation.tailrec
import cats.Monoid
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

    def sum[A: Monoid](xs: List[A]): A = {
      import cats.syntax.monoid._

      @tailrec
      def sum(xs: List[A], acc: A): A = xs match {
        case Nil => acc
        case Cons(h, t) => sum(t, acc |+| h) // Monoid[A].combine(acc, h)
      }

      sum(xs, Monoid[A].empty)
    }

    def sumFoldLeft[A: Monoid](xs: List[A]): A = {
      import cats.syntax.monoid._

      foldLeft(xs, Monoid[A].empty)(_ |+| _) // Monoid[A].combine
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def productFoldLeft[A](xs: List[A])(implicit M: Monoid[A]): A = {
      import cats.syntax.monoid._

      foldLeft(xs, M.empty)(_ |+| _) // M.combine
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

    def appendUsingFoldLeft[A](xs: List[A], ys: List[A]): List[A] =
      foldLeft(reverse(ys), xs)((xs, y) => Cons(y, xs))

    def appendUsingFoldRight[A](xs: List[A], ys: List[A]): List[A] =
      foldRight(xs, ys)((x, ys) => Cons(x, ys))

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

    def foldRight[A, B](xs: List[A], z: B)(f: (A, B) => B): B = xs match {
      case Nil =>
        z

      case Cons(h, t) =>
        println(s"foldRight that will not short circuit for h = $h")
        f(h, foldRight(t, z)(f))
    }

    def foldLeft[A, B](xs: List[A], z: B)(f: (B, A) => B): B = {
      @tailrec
      def foldLeft(xs: List[A], acc: B): B = xs match {
        case Nil => acc
        case Cons(h, t) => foldLeft(t, f(acc, h))
      }

      foldLeft(xs, z)
    }

    def foldLeftUsingFoldRight[A, B](xs: List[A], z: B)(f: (B, A) => B): B = {
      // Easy way - just use reverse
      // foldRight(reverse(xs), z)((a, b) => f(b, a))

      // Other implementation builds up a chain of functions which, when called,
      // results in the operations being performed with the correct associativity.
      // We are calling `foldRight` with the `B` type being instantiated to `B => B`,
      // then calling the built up function with the `z` argument.
      def g: B => B = foldRight(xs, (b: B) => b)((a, g: B => B) =>
        b => g(f(b, a))
      )

      g(z)
    }

    def foldRightUsingFoldLeft[A, B](xs: List[A], z: B)(f: (A, B) => B): B = {
      // Easy way - just use reverse
      // foldLeft(reverse(xs), z)((b, a) => f(a, b))

      // Other implementation builds up a chain of functions which, when called,
      // results in the operations being performed with the correct associativity.
      // We are calling `foldLeft` with the `B` type being instantiated to `B => B`,
      // then calling the built up function with the `z` argument.
      def g: B => B = foldLeft(xs, (b: B) => b)((g: B => B, a) =>
        b => g(f(a, b))
      )

      g(z)
    }

    def length[A](xs: List[A]): Int = {
      @tailrec
      def length(xs: List[A], count: Int): Int = xs match {
        case Nil => count
        case Cons(_, t) => length(t, count + 1)
      }

      length(xs, 0)
    }

    def lengthFoldLeft[A](xs: List[A]): Int =
      foldLeft(xs, 0)((count, _) => count + 1)

    def reverse[A](xs: List[A]): List[A] =
      foldLeft(xs, Nil: List[A])((acc, x) => Cons(x, acc))

    def concat[A](xs: List[List[A]]): List[A] =
      foldLeftUsingFoldRight(xs, Nil: List[A])(append)
  }

  "3.1 What will be the result of the following match expression?" in {
    import cats.instances.int._
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

  "3.7 Short circuit foldRight" in {
    // Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it encounters a 0.0?

    import List._

    val xs = List(1, 2, 3, 0, 5)

    // Original implementation does not short circuit
    foldRight(xs, 1)((a, b) => a * b) mustBe 0
  }

  "3.8" in {
    import List._

    val xs = List(1, 2, 3, 4, 5)

    foldRight(xs, Nil: List[Int])(Cons(_, _)) mustBe xs
  }

  "3.9 Compute length of a list using foldRight" in {
    val xs = List(1, 2, 3, 4, 5)

    List.length(xs) mustBe 5
  }

  "3.10 foldLeft" in {
    import List._

    val xs = List(1, 2, 3, 4, 5)

    foldLeft(xs, 0)(_ + _) mustBe 15
  }

  "3.11 sum, product, length using foldLeft" in {
    import cats.instances.int._
    import List._

    val xs = List(1, 2, 3, 4, 5)

    sumFoldLeft(xs) mustBe 15
    productFoldLeft(xs)(IntProductMonoid) mustBe 120
    lengthFoldLeft(xs) mustBe 5

    object IntProductMonoid extends Monoid[Int] {
      def empty: Int = 1

      def combine(x: Int, y: Int): Int = x * y
    }
  }

  "3.12" in {
    import List._

    reverse(List(1, 2, 3, 4, 5)) mustBe List(5, 4, 3, 2, 1)
  }

  "3.13 foldLeft in terms of foldRight and vice versa" in {
    import List._

    val xs = List(1, 2, 3, 4, 5)

    foldLeftUsingFoldRight(xs, 0)((acc, x) => acc + x) mustBe 15
    foldLeftUsingFoldRight(xs, 1)((acc, x) => acc * x) mustBe 120
    foldLeftUsingFoldRight(List("h", "e", "l", "l", "o"), "")((acc, x) => acc + x) mustBe "hello"

    foldRightUsingFoldLeft(xs, 0)((x, acc) => x + acc) mustBe 15
    foldRightUsingFoldLeft(xs, 1)((x, acc) => x * acc) mustBe 120
    foldRightUsingFoldLeft(List("h", "e", "l", "l", "o"), "")((x, acc) => x + acc) mustBe "hello"
  }

  "3.14 Implement append in terms of either foldLeft or foldRight" in {
    import List._

    val xs = List(1, 2, 3, 4, 5)

    append(xs, xs) mustBe List(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)
    appendUsingFoldLeft(xs, xs) mustBe List(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)
    appendUsingFoldRight(xs, xs) mustBe List(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)
  }

  "3.15 Concatenate a list of lists into a single list" in {
    import List._

    val xs = List(
      List(1, 2, 3),
      List(4),
      List(5, 6)
    )

    concat(xs) mustBe List(1, 2, 3, 4, 5, 6)
  }

  "3.16" in {

  }

  "3.24" in {

  }
}