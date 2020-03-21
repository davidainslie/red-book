package com.backwards.chap5

import scala.annotation.tailrec
import scala.{Stream => _} // Hide std library `Stream` since we are writing our own, though Stream in 2.13.x has be deprecated in favour of LazyList
import cats.implicits._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.chap5.Stream._

class Chap5Spec extends AnyWordSpec with Matchers {
  "Stream" should {
    "Memoize via smart constructor" in {
      // Without smart constructor, an expensive operation can be evaluated more than once:
      def expensive(msg: String): String = {
        println(msg)
        msg
      }

      val x = Cons(() => expensive("Without smart constructor"), () => Stream("one", "two"))
      val h1 = x.headOption
      val h2 = x.headOption
      println(x)

      // With smart constructor:
      val y = cons(expensive("With smart constructor"), Stream("one", "two"))
      val h11 = y.headOption
      val h12 = y.headOption
      println(y)
    }

    "5.1 convert to list" in {
      val stream: Stream[Int] = Stream(1, 2, 3, 4, 5)

      stream.toList mustBe List(1, 2, 3, 4, 5)
    }

    "5.2a take(n) - returning the first n elements of a Stream" in {
      val stream: Stream[Int] = Stream(1, 2, 3, 4, 5)

      stream.take(3).toList mustBe List(1, 2, 3)
      stream.take(10).toList mustBe stream.toList
      stream.take(0) mustBe Empty
      Stream().take(10) mustBe Empty
    }

    "5.2b drop(n) - skipping the first n elements of a Stream" in {
      val stream: Stream[Int] = Stream(1, 2, 3, 4, 5)

      stream.drop(2).toList mustBe List(3, 4, 5)
      stream.drop(10) mustBe Empty
      stream.drop(0).toList mustBe stream.toList
      Stream().drop(10) mustBe Empty
    }

    "5.3 takeWhile - returning all starting elements of a Stream that match a given predicate" in {
      val stream: Stream[Int] = Stream(1, 2, 3, 4, 5)

      stream.takeWhile(_ < 4).toList mustBe List(1, 2, 3)
      stream.takeWhile(_ > 10) mustBe Empty
    }
  }
}

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h()) // Explicit forcing of the h thunk using h()
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() +: t().toList
  }

  def toListTailRecursive: List[A] = {
    @tailrec
    def toList(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => toList(t(), acc :+ h())
    }

    toList(this, Nil)
  }

  // Make sure that the tail is not invoked unless we need to, by handling the special case where n == 1 separately
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty =>
      empty
    case Cons(h, t) =>
      val hApplied = h()

      if (p(hApplied)) cons(hApplied, t().takeWhile(p)) else empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  // A smart constructor for creating a nonempty stream.
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    // We cache the head and tail as lazy values to avoid repeated evaluation.
    lazy val head = hd
    lazy val tail = tl

    Cons(() => head, () => tail)
  }

  // A smart constructor for creating an empty stream of a particular type.
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}