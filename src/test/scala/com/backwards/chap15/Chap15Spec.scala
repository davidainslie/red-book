package com.backwards.chap15

import cats.effect.IO
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scala.io.Source

class Chap15Spec extends AnyWordSpec with Matchers {
  "Counting line numbers in imperative style embedded within IO Monad" should {
    "look ugly" in {
      /**
       * The Good:
       * - It's incremental: the entire file isn’t loaded into memory up front.
       *   Instead, lines are fetched from the file only when needed.
       *   If we didn’t buffer the input, we could keep as little as a single line of the file in memory at a time.
       * - It also terminates early, as soon as the answer is known.
       *
       * The Bad:
       * - We have to remember to close the file when we’re done.
       *   This might seem obvious, but if we forget to do this, or (more commonly) if we close the file outside of a finally block
       *   and an exception occurs first, the file will remain open.
       *   This is called a resource leak.
       * - It entangles the high-level algorithm with low-level concerns about iteration and file access.
       * - What if we want to check whether the number of nonempty lines in the file exceeds 40,000?
       * - Or find a line index before 40,000 where the first letters of consecutive lines spell out "abracadabra".
       */
      def linesGt40k(filename: String): IO[Boolean] = IO {
        val src = Source.fromFile(filename)

        try {
          var count = 0
          val lines: Iterator[String] = src.getLines

          while (count <= 40000 && lines.hasNext) {
            lines.next
            count += 1
          }

          count > 40000
        }

        finally src.close
      }

      linesGt40k("build.sbt").unsafeRunSync


      // We not only want resource management but also allow for extension of code as suggested above e.g.

      // Check whether the number of nonempty lines in the file exceeds 40,000:
      val src = Source.fromFile("build.sbt")
      val lines: Iterator[String] = src.getLines

      lines.zipWithIndex.exists(_._2 + 1 >= 40000)

      // and if we consider empty lines:
      lines.filterNot(_.trim.nonEmpty).zipWithIndex.exists(_._2 + 1 >= 40000)
    }
  }

  "Process" should {
    "liftOne" in new Version1 {
      import Process._

      val p: Process[Int, Int] = liftOne((x: Int) => x * 2)

      val xs: List[Int] = p(LazyList(1, 2, 3)).toList

      xs mustBe List(2)

      /*
      As we can see, this Process just waits for one element, emits it, and then stops.
      To transform a whole stream with a function, we do this repeatedly in a loop, alternating between awaiting and emitting.
      We can write a combinator for this, repeat, as a method on Process.
      */
    }

    "filter" in new Version1 {
      import Process._

      val even: Process[Int, Int] = filter((x: Int) => x % 2 == 0)

      val evens: List[Int] = even(LazyList(1, 2, 3, 4)).toList

      evens mustBe List(2, 4)
    }

    "sum" in new Version1 {
      import Process._

      val s: List[Double] = sum(LazyList(1.0, 2.0, 3.0, 4.0)).toList

      s mustBe List(1.0, 3.0, 6.0, 10.0)
    }

    "15.1a take" in new Version1 {

    }

    "15.1b drop" in new Version1 {

    }

    "15.1c takeWhile" in new Version1 {

    }

    "15.1d dropWhile" in new Version1 {

    }

    "15.2 count" in new Version1 {

    }

    "15.3 mean" in new Version1 {

    }

    "15.4a sum using loop" in new Version1 {

    }

    "15.4b count using loop" in new Version1 {

    }
  }
}

trait Version1 {
  /**
   * A Process[I, O] can be used to transform a stream containing I values to a stream of O values.
   * But Process[I, O] isn’t a typical function Stream[I] => Stream[O], which could consume the input stream and construct the output stream.
   * Instead, we have a state machine that must be driven forward with a driver, a function that simultaneously consumes both our Process and the input stream.
   * A Process can be in one of three states, each of which signals something to the driver.
   */
  sealed trait Process[I, O] {
    def apply(s: LazyList[I]): LazyList[O] = this match {
      case Halt() =>
        LazyList()

      case Await(recv) => s match {
        case h #:: t =>
          recv(Some(h))(t)
        case xs =>
          recv(None)(xs) // Stream is empty
      }

      case Emit(h, t) =>
        h #:: t(s)
    }

    def repeat: Process[I, O] = {
      def go(p: Process[I, O]): Process[I, O] = p match {
        case Halt() => go(this) // Restart the process if it halts on its own.

        case Await(recv) => Await {
          case None => recv(None) // Don’t repeat if terminated from source.
          case i => go(recv(i))
        }

        case Emit(h, t) => Emit(h, go(t))
      }

      go(this)
    }
  }

  /**
   * Emit(head, tail) indicates to the driver that the head value should be emitted to the output stream, and the machine should then be transitioned to the tail state.
   * We use a default argument so that we can say Emit(xs) as shorthand for Emit(xs, Halt())
   */
  case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]

  /**
   * Await(recv) requests a value from the input stream. The driver should pass the next available value to the recv function, or None if the input has no more elements.
   */
  case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]

  /**
   * Halt indicates to the driver that no more elements should be read from the input or emitted to the output.
   */
  case class Halt[I, O]() extends Process[I, O]

  object Process {
    def emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()): Process[I, O] =
      Emit(head, tail)

    /**
     * A helper function to await an element or fall back to another process if there is no input.
     */
    def await[I, O](f: I => Process[I, O], fallback: Process[I, O] = Halt[I, O]()): Process[I, O] =
      Await[I, O] {
        case Some(i) => f(i)
        case None => fallback
      }

    // We can convert any function f: I => O to a Process[I,O].
    // We just Await and then Emit the value received, transformed by f:
    def liftOne[I, O](f: I => O): Process[I, O] = Await {
      case Some(i) =>
        Emit(f(i))
      case None =>
        Halt()
    }

    // We can lift any function to a Process that maps over a Stream:
    def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

    // Here’s a Process that filters out elements that don’t match the predicate p:
    def filter[I](p: I => Boolean): Process[I, I] = Await[I, I] {
      case Some(i) if p(i) => emit(i)
      case _ => Halt()
    }.repeat

    def sum: Process[Double, Double] = {
      def go(acc: Double): Process[Double, Double] =
        Await {
          case Some(d) => Emit(d+acc, go(d+acc))
          case None => Halt()
        }

      go(0.0)
    }

    def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
      await((i: I) => f(i, z) match {
        case (o, s2) => emit(o, loop(s2)(f))
      })
  }
}