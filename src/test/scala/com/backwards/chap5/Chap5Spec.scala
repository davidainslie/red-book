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

    """5.4 forAll - checks that all elements in the Stream match a given predicate.
       The implementation should terminate the traversal as soon as it encounters a nonmatching value.""" in {

      val stream: Stream[Int] = Stream(1, 2, 3, 4, 5)

      stream.forAll(_ < 6) mustBe true
      stream.forAll(_ < 4) mustBe false
      Stream[Int]().forAll(_ == 1) mustBe false
    }

    "5.5 takeWhile using foldRight" in {
      val stream: Stream[Int] = Stream(1, 2, 3, 4, 5)

      stream.takeWhileUsingFoldRight(_ < 4).toList mustBe List(1, 2, 3)
      stream.takeWhileUsingFoldRight(_ > 10) mustBe Empty
    }

    "5.6 headOption using foldRight" in {
      val stream: Stream[Int] = Stream(1, 2, 3, 4, 5)

      stream.headOption mustBe Option(1)
      Stream().headOption mustBe None
    }

    "5.7a map using foldRight" in {
      val stream: Stream[Int] = Stream(1, 2, 3, 4, 5)

      stream.map(_ + 10).toList mustBe List(11, 12, 13, 14, 15)
      Stream[Int]().map(_ + 10) mustBe Empty
    }

    "5.7b filter using foldRight" in {
      val stream: Stream[Int] = Stream(1, 2, 3, 4, 5)

      stream.filter(_ > 3).toList mustBe List(4, 5)
      Stream[Int]().filter(_ > 3) mustBe Empty
    }

    "5.7c append using foldRight which should be non-strict in its argument" in {
      val stream: Stream[Int] = Stream(1, 2, 3, 4, 5)

      stream.append(Stream(10, 11)).toList mustBe List(1, 2, 3, 4, 5, 10, 11)
      Stream[Int]().append(Stream(10, 11)).toList mustBe List(10, 11)
      stream.append(Stream[Int]()).toList mustBe List(1, 2, 3, 4, 5)
    }

    "5.7d flatMap using foldRight" in {
      val stream: Stream[Int] = Stream(1, 2, 3, 4, 5)

      stream.flatMap(i => Stream(i + 10)).toList mustBe List(11, 12, 13, 14, 15)
      Stream[Int]().flatMap(i => Stream(i + 10)) mustBe Empty
    }

    "infinite stream" in {
      lazy val ones: Stream[Int] = cons(1, ones)

      ones.take(5).toList mustBe List(1, 1, 1, 1, 1)

      ones.map(_ + 1).exists(_ % 2 == 0) mustBe true

      ones.takeWhile(_ == 1) mustBe a [Stream[_]]

      // ones.forAll(_ != 1) Results in infinite recursion causing a stack overflow
    }

    "5.8 generalize ones to the function constant, which returns an infinite Stream of a given value" in {
      lazy val ones: Stream[Int] = constant(1)

      ones.take(5).toList mustBe List(1, 1, 1, 1, 1)
    }

    "5.9 function that generates an infinite stream of integers, starting from n, then n + 1, n + 2, and so on" in {
      lazy val ascending: Stream[Int] = from(5)

      ascending.take(5).toList mustBe List(5, 6, 7, 8, 9)
    }

    "5.10 function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on" in {
      fibs.take(7).toList mustBe List(0, 1, 1, 2, 3, 5, 8)
    }

    "5.11 unfold - a more general stream-building function that takes an initial state, and a function for producing both the next state and the next value in the generated stream" in {
      unfold[String, Int](5)(_ => None) mustBe Empty

      unfold[String, Int](5)(state => Option((s"ye-$state", state + 1))).take(3).toList mustBe List("ye-5", "ye-6", "ye-7")
    }

    "5.12a ones using unfold" in {
      ones.take(5).toList mustBe List(1, 1, 1, 1, 1)
    }

    "5.12b constant using unfold" in {
      constantUsingUnfold(4).take(3).toList mustBe List(4, 4, 4)
    }

    "5.12c from using unfold" in {
      fromUsingUnfold(10).take(5).toList mustBe List(10, 11, 12, 13, 14)
    }

    "5.12d fibs using unfold" in {
      fibsUsingUnfold.take(7).toList mustBe List(0, 1, 1, 2, 3, 5, 8)
    }

    "5.13a map using unfold" in {
      val stream: Stream[Int] = Stream(1, 2, 3, 4, 5)

      stream.mapUsingUnfold(_ + 10).toList mustBe List(11, 12, 13, 14, 15)
      Stream[Int]().mapUsingUnfold(_ + 10) mustBe Empty
    }

    "5.13b take using unfold" in {
      val stream: Stream[Int] = Stream(1, 2, 3, 4, 5)

      stream.takeUsinUnfold(3).toList mustBe List(1, 2, 3)
      stream.takeUsinUnfold(10).toList mustBe stream.toList
      stream.takeUsinUnfold(0) mustBe Empty
      Stream().takeUsinUnfold(10) mustBe Empty
    }

    "5.13c takeWhile using unfold" in {
      val stream: Stream[Int] = Stream(1, 2, 3, 4, 5)

      stream.takeWhileUsingUnfold(_ < 4).toList mustBe List(1, 2, 3)
      stream.takeWhileUsingUnfold(_ > 10) mustBe Empty
    }

    "5.13d zipWith using unfold" in {
      val stream1: Stream[Int] = Stream(1, 2, 3)
      val stream2: Stream[String] = Stream("4", "5", "6")

      stream1.zipWith(stream2)(_ -> _).toList mustBe List(1 -> "4", 2 -> "5", 3 -> "6")

      stream1.drop(1).zipWith(stream2)(_ -> _).toList mustBe List(2 -> "4", 3 -> "5")
    }

    """5.13e zipAll using unfold
      The zipAll function should continue the traversal as long as either stream has more elements—it uses Option to indicate whether each stream has been exhausted.
    """ in {
      val stream1: Stream[Int] = Stream(1, 2, 3)
      val stream2: Stream[String] = Stream("4", "5", "6")

      stream1.zipAll(stream2).toList mustBe List(Option(1) -> Option("4"), Option(2) -> Option("5"), Option(3) -> Option("6"))

      stream1.drop(3).zipAll(stream2).toList mustBe List(None -> Option("4"), None -> Option("5"), None -> Option("6"))
    }

    "5.14 startsWith - check if one Stream is a prefix of another. For instance, Stream(1, 2, 3) startsWith Stream(1, 2) would be true" in {
      val source = Stream(1, 2, 3)
      val target = Stream(1, 2)

      source.startsWith(target) mustBe true
    }

    "5.15a tails - for a given Stream, tails returns the Stream of suffixes of the input sequence e.g. given Stream(1, 2, 3), it would return Stream(Stream(1, 2, 3), Stream(2, 3), Stream(3), Stream())" in {
      val stream = Stream(1, 2, 3)

      stream.tails.toList.map(_.toList) mustBe List(List(1, 2, 3), List(2, 3), List(3), Nil)
    }

    "5.15b hasSubsequence" in {
      val stream = Stream(1, 2, 3, 4, 5)

      stream.hasSubsequence(Stream(3, 4)) mustBe true
      stream.hasSubsequence(Stream(1, 3)) mustBe false
      stream.hasSubsequence(Stream("a")) mustBe false
    }

    "5.16 scanRight that generalises tails, which is like a foldRight that returns a stream of the intermediate results" in {
      val stream = Stream(1, 2, 3)

      stream.scanRight(0)(_ + _).toList mustBe List(6, 5, 3, 0)
    }
  }
}

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h()) // Explicit forcing of the h thunk using h()
  }

  def headOptionUsingFoldRight: Option[A] =
    foldRight[Option[A]](None)((a, _) => Option(a))

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

  def takeUsinUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), n) if n > 0 =>
        Option(h(), (t(), n - 1))

      case _ =>
        None
    }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty =>
      empty
    case Cons(h, t) =>
      val hApplied = h()

      if (p(hApplied)) cons(hApplied, t().takeWhile(p)) else empty
  }

  def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] = this match {
    case Empty =>
      empty

    case _ =>
      foldRight(empty[A]) { (a, b) =>
        val aApplied = p(a)

        if (aApplied) cons(a, b)
        else empty
      }
  }

  def takeWhileUsingUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Empty =>
        None

      case Cons(h, t) =>
        val hApplied = h()

        if (p(hApplied)) Option((hApplied, t()))
        else None
    }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
    case _ => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => false
    case _ => foldRight(true)((a, b) => p(a) && b) // Since `&&` is non-strict in its second argument, this terminates the traversal as soon as a nonmatching element is found.
  }

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def mapUsingUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Option(f(h()), t())
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A]) { (a, b) =>
      if (p(a)) cons(a, b)
      else b
    }

  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append b)

  // Since intermediate streams aren’t instantiated, it’s easy to reuse existing combina-tors in novel ways
  // without having to worry that we’re doing more processing of the stream than necessary.
  // E.g. we can reuse filter to define find, a method to return just the first element that matches if it exists.
  // Even though filter transforms the whole stream, that transformation is done lazily, so find terminates as soon as a match is found:
  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Option(f(h1(), h2()), t1() -> t2())

      case _ =>
        None
    }

  // Special case of `zipWith`
  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipWith(s2)(_ -> _)

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold(this, s2) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Option(Option(h1()) -> Option(h2()), t1() -> t2())

      case (Cons(h1, t1), _) =>
        Option(Option(h1()) -> (None: Option[B]), t1() -> Empty)

      case (_, Cons(h2, t2)) =>
        Option((None: Option[A]) -> Option(h2()), Empty -> t2())

      case _ =>
        None
    }

  def startsWithRubbish[B >: A](s: Stream[B]): Boolean = {
    @tailrec
    def startsWith(source: Stream[A], target: Stream[B]): Boolean = (source, target) match {
      case (Cons(sh, st), Cons(th, tt)) if sh() == th() => startsWith(st(), tt())
      case (Cons(_, _), Empty) => true
      case (Empty, Cons(_, _)) => false
      case (Empty, Empty) => true
    }

    startsWith(this, s)
  }

  def startsWith[B](s: Stream[B]): Boolean = {
    zipAll(s) takeWhile { case (_, b) =>
      b.isDefined
    } forAll { case (a, b) =>
      a == b
    }
  }

  // The last element of `tails` is always the empty `Stream`, so we handle this as a special case, by appending it to the output.
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case c @ Cons(_, t) => Option((c, t()))
    } append Stream(empty)

  def hasSubsequence[B](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    val (_, output) = foldRight((z, Stream(z)))((a, acc) => {
      lazy val (z, accBreadCrumbs) = acc
      val newAcc = f(a, z)
      (newAcc, cons(newAcc, accBreadCrumbs))
    })

    output
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  // A smart constructor for creating a nonempty stream.
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    // We cache the head and tail as lazy values to avoid repeated evaluation.
    lazy val head = hd
    lazy val tail = tl

    Cons(() => head, () => tail)
  }

  // A smart constructor for creating an empty stream of a particular type.
  def empty[A]: Stream[A] = Empty

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def constantUsingUnfold[A](a: A): Stream[A] =
    unfold(a)(state => Option(state, state))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fromUsingUnfold(n: Int): Stream[Int] =
    unfold(n)(state => Option(state, state + 1))

  def fibs: Stream[Int] = {
    def fibs(x: Int, y: Int): Stream[Int] =
      cons(y, fibs(y, x + y))

    cons(0, fibs(0, 1))
  }

  def fibsAlternative: Stream[Int] = {
    def fibs(x: Int, y: Int): Stream[Int] =
      cons(y, fibs(y, x + y))

    fibs(0, 1)
  }

  def fibsUsingUnfold: Stream[Int] =
    unfold((0, 1)) { case (x, y) =>
      Option(x, (y, x + y))
    }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty[A]
      case Some((a, next)) => cons(a, unfold(next)(f))
    }

  def ones: Stream[Int] =
    unfold(1)(state => Option(state, state))
}