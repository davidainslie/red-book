package com.backwards.chap15

import java.io.File
import scala.annotation.tailrec
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
      import Process._

      val process: Process[Int, Int] = take[Int](2)

      process(LazyList(1, 2, 3, 4)).toList mustBe List(1, 2)
    }

    "15.1b drop" in new Version1 {
      import Process._

      val process: Process[Int, Int] = drop[Int](1)

      process(LazyList(1, 2, 3, 4)).toList mustBe List(2, 3, 4)
    }

    "15.1c takeWhile" in new Version1 {
      import Process._

      val process: Process[Int, Int] = takeWhile[Int](_ < 4)

      process(LazyList(1, 2, 3, 4)).toList mustBe List(1, 2, 3)
    }

    "15.1d dropWhile" in new Version1 {
      import Process._

      val process: Process[Int, Int] = dropWhile[Int](_ < 3)

      process(LazyList(1, 2, 3, 4)).toList mustBe List(3, 4)
    }

    "15.2 count" in new Version1 {
      import Process._

      val process: Process[Int, Int] = count[Int]

      process(LazyList(1, 2, 3, 4)).last mustBe 4
    }

    "15.3 mean" in new Version1 {
      import Process._

      val process: Process[Double, Double] = mean

      process(LazyList(1, 2, 3, 4)).toList mustBe List(1.0, 1.5, 2.0, 2.5)
      process(LazyList(1, 2, 3, 4)).last mustBe 2.5
    }

    "15.4a sum using loop" in new Version1 {
      import Process._

      val s: List[Double] = sumUsingLoop(LazyList(1.0, 2.0, 3.0, 4.0)).toList

      s mustBe List(1.0, 3.0, 6.0, 10.0)
    }

    "15.4b count using loop" in new Version1 {
      import Process._

      val process: Process[Int, Int] = countUsingLoop[Int]

      process(LazyList(1, 2, 3, 4)).last mustBe 4
    }

    "15.5 pipe" in new Version1 {
      import Process._

      val p1: Process[Double, Double] = sum

      val p2: Process[Double, Double] = sum

      val p: Process[Double, Double] = p1 |> p2

      p(LazyList(1.0, 2.0, 3.0, 4.0)).last mustBe 20.0

      // To filter and map with a single transformation:
      (filter[Int](_ % 2 == 0) |> lift(_ + 1))(LazyList(1, 2, 3, 4)).toList mustBe List(3, 5)
    }

    "15.6 zipWithIndex" in new Version1 {
      import Process._
    }

    "15.7 mean in terms of a combinator using sum and count" in new Version1 {
      import Process._
    }

    "15.8 exists" in new Version1 {
      import Process._
    }

    "15.9 toCelsius" in new Version1 {
      import Process._
    }
  }

  "Back to counting line numbers but now with monadic streams" should {
    "boil down the counting example to: count |> exists(_ > 40000)" in new Version1 {
      import Process._

      def processFile[A, B](
        file: File,
        process: Process[String, A],
        z: B
      )(g: (B, A) => B): IO[B] = IO {
        @tailrec
        def go(ss: Iterator[String], cur: Process[String, A], acc: B): B =
          cur match {
            case Halt() =>
              acc

            case Await(recv) =>
              val next = if (ss.hasNext) recv(Some(ss.next)) else recv(None)
              go(ss, next, acc)

            case Emit(h, t) =>
              go(ss, t, g(acc, h))
          }

        val s = Source.fromFile(file)

        try go(s.getLines, process, z) finally s.close
      }

      // We can now solve the original problem with the following:
      processFile(new File("build.sbt"), count |> exists(_ > 40000), false)(_ || _)

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
    import Process._

    def apply(s: LazyList[I]): LazyList[O] = this match {
      case Halt() =>
        println(s"Apply halt")
        LazyList()

      case Await(recv) => s match {
        case h #:: t =>
          println(s"Apply await: head = $h, tail = $t")
          recv(Some(h))(t)
        case xs =>
          println(s"Apply empty stream")
          recv(None)(xs) // Stream is empty
      }

      case Emit(h, t) =>
        println(s"Apply emit: head = $h, tail = $t")
        h #:: t(s)
    }

    def repeat: Process[I, O] = {
      def go(p: Process[I, O]): Process[I, O] = p match {
        case Halt() =>
          println(s"Repeat halt -> restart $this")
          go(this) // Restart the process if it halts on its own.

        case Await(recv) =>
          println(s"Repeat: await with recv = $recv")
          Await {
            case None =>
              println(s"Repeat applying await to NOT repeat")
              recv(None) // Don’t repeat if terminated from source.
            case i =>
              println(s"Repeat applying await for $i to go again with recv evaluated to ${recv(i)}")
              go(recv(i))
          }

        case Emit(h, t) =>
          println(s"Repeat emit: head = $h, tail = $t, and applying go to tail")
          Emit(h, go(t))
      }

      go(this)
    }

    def |>[O2](p2: Process[O, O2]): Process[I, O2] = p2 match {
      case Halt() =>
        Halt()

      case Emit(h, t) =>
        Emit(h, this |> t)

      case Await(f) => this match {
        case Emit(h, t) =>
          t |> f(Some(h))

        case Halt() =>
          Halt() |> f(None)

        case Await(g) =>
          Await((i: Option[I]) => g(i) |> p2)
      }
    }

    // This means that the type constructor Process[I, _] is a functor.
    // If we ignore the input side I for a moment, we can just think of a Process[I, O] as a sequence of O values.
    // This implementation of map is then analogous to mapping over a Stream or a List.
    def map[O2](f: O => O2): Process[I,O2] =
      this |> lift(f)

    def ++(p: => Process[I, O]): Process[I, O] = this match {
      case Halt() => p
      case Emit(h, t) => Emit(h, t ++ p)
      case Await(recv) => Await(recv andThen (_ ++ p))
    }

    def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
      case Halt() => Halt()
      case Emit(h, t) => f(h) ++ t.flatMap(f)
      case Await(recv) => Await(recv andThen (_ flatMap f))
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
    // def monad[I]: Monad[({ type f[x] = Process[I,x] })#f]
    def monad[I]: Monad[Process[I, *]] =
      // new Monad[({ type f[x] = Process[I,x] })#f]
      new Monad[Process[I, *]] {
        def unit[O](o: => O): Process[I, O] =
          emit(o)

        def flatMap[O, O2](p: Process[I, O])(f: O => Process[I, O2]): Process[I, O2] =
          p flatMap f
      }

    // Enable monadic syntax for `Process` type
    implicit def toMonadic[I, O](a: Process[I, O]): Monadic[Process[I, *], O] =
      monad[I].toMonadic(a)


    def emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()): Process[I, O] = {
      println(s"New emit: head = $head, tail = $tail")
      Emit(head, tail)
    }

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

    // The identity `Process`, just repeatedly echos its input.
    def id[I]: Process[I,I] = lift(identity)

    def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
      await((i: I) => f(i, z) match {
        case (o, s2) => emit(o, loop(s2)(f))
      })

    // Here’s a Process that filters out elements that don’t match the predicate p:
    def filter[I](p: I => Boolean): Process[I, I] = Await[I, I] {
      case Some(i) if p(i) =>
        println(s"Filter: $i")
        emit(i)
      case _ =>
        println("Filter halt")
        Halt()
    }.repeat

    def sum: Process[Double, Double] = {
      def go(acc: Double): Process[Double, Double] = Await {
        case Some(d) => Emit(d + acc, go(d + acc))
        case None => Halt()
      }

      go(0.0)
    }

    def sumUsingLoop: Process[Double, Double] =
      loop(0.0)((d: Double, acc) => (acc + d, acc + d))

    def take[I](n: Int): Process[I, I] =
      if (n <= 0) Halt()
      else await(i => emit(i, take(n - 1)))

    def takeWhile[I](f: I => Boolean): Process[I, I] =
      await { i =>
        if (f(i)) emit(i, takeWhile(f))
        else Halt()
      }

    def drop[I](n: Int): Process[I, I] =
      if (n <= 0) id
      else await(_ => drop(n - 1))

    def dropWhile[I](f: I => Boolean): Process[I, I] =
      await { i =>
        if (f(i)) dropWhile(f)
        else emit(i, id)
      }

    /*
     * Here's one implementation, with three stages.
     * We map all inputs to 1.0, compute a running sum, then finally convert the output back to `Int`.
     * The three stages will be interleaved - as soon as the first element is examined, it will be converted to 1.0,
     * then added to the running total, and then this running total will be converted back to `Int`,
     * then the `Process` will examine the next element, and so on.
     */
    def count[I]: Process[I, Int] =
      lift((i: I) => 1.0) |> sum |> lift(_.toInt)

    /* For comparison, here is an explicit recursive implementation. */
    def count2[I]: Process[I, Int] = {
      def go(n: Int): Process[I, Int] =
        await((i: I) => emit(n + 1, go(n + 1)))

      go(0)
    }

    def countUsingLoop[I]: Process[I, Int] =
      loop(0)((_: I, n) => (n + 1, n + 1))

    def mean: Process[Double, Double] = {
      def go(sum: Double, count: Double): Process[Double, Double] =
        await((d: Double) => emit((sum + d) / (count + 1), go(sum + d, count + 1)))

      go(0.0, 0.0)
    }
  }
}