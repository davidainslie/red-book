package com.backwards.chap13

import scala.io.StdIn
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.chap12.Monad

/**
 * We want more functions pure and push side effects to the outer layers.
 * We could call these impure functions the “imperative shell” around the pure “core” of the program.
 * Eventually, we reach functions that seem to necessitate side effects like the built-in println, which has type String => Unit.
 *
 * Given an impure function f of type A => B, we can split f into two functions:
 * - A pure function of type A => D, where D is some description of the result of f.
 * - An impure function of type D => B, which can be thought of as an interpreter of these descriptions.
 */
class Chap13Spec extends AnyWordSpec with Matchers {
  case class Player(name: String, score: Int)

  "Program with side effects - without using IO" should {
    "incorrectly mix it all up" in {
      def contest(p1: Player, p2: Player): Unit =
        if (p1.score > p2.score)      println(s"${p1.name} is the winner!")
        else if (p2.score > p1.score) println(s"${p2.name} is the winner!")
        else                          println("It's a draw.")
    }

    "take a step to refactoring the mess" in {
      def winner(p1: Player, p2: Player): Option[Player] =
        if (p1.score > p2.score)      Some(p1)
        else if (p1.score < p2.score) Some(p2)
        else                          None

      def contest(p1: Player, p2: Player): Unit =
        winner(p1, p2) match {
          case Some(Player(name, _)) => println(s"$name is the winner!")
          case None => println("It's a draw.")
        }
    }

    "take another step" in {
      def winner(p1: Player, p2: Player): Option[Player] =
        if (p1.score > p2.score)      Some(p1)
        else if (p1.score < p2.score) Some(p2)
        else                          None

      def winnerMsg(p: Option[Player]): String =
        p map {
          case Player(name, _) => s"$name is the winner!"
        } getOrElse "It's a draw."

      def contest(p1: Player, p2: Player): Unit =
        println(winnerMsg(winner(p1, p2)))
    }
  }

  trait FirstAttempt {
    // With function "empty" and the binary function "++" we can already say that IO forms a Monoid
    // So if we have, for example, a List[IO], we can reduce that to a single IO, and the associativity of ++ means that we can do this either by folding left or folding right.
    trait IO {
      self =>
      def run(): Unit

      def ++(io: IO): IO = () => {
        self.run()
        io.run()
      }
    }

    object IO {
      def empty: IO = () => ()
    }

    def PrintLine(msg: String): IO =
      new IO {
        def run(): Unit = println(msg)
      }
  }

  "Program with side effects - using IO" should {
    "apply first attempt" in new FirstAttempt {
      def winner(p1: Player, p2: Player): Option[Player] =
        if (p1.score > p2.score)      Some(p1)
        else if (p1.score < p2.score) Some(p2)
        else                          None

      def winnerMsg(p: Option[Player]): String =
        p map {
          case Player(name, _) => s"$name is the winner!"
        } getOrElse "It's a draw."

      def contest(p1: Player, p2: Player): IO =
        PrintLine(winnerMsg(winner(p1, p2)))

      // Unlike previously, our contest function is now pure.
      // It returns an IO value, which simply describes an action that needs to take place, but doesn’t actually execute it.
      // We say that contest has (or produces) an effect or is effectful, but it’s only the interpreter of IO (its run method) that actually has a side effect.

      // Now contest only has one responsibility, which is to compose the parts of the program together:
      // - winner to compute who the winner is
      // - winnerMsg to compute what the resulting message should be
      // - and PrintLine to indicate that the message should be printed to the console

      // But the responsibility of interpreting the effect and actually manipulating the console is held by the run method on IO.
    }
  }

  "Imperative program that converts Fahrenheit to Celsius" should {
    "be for example" in new FirstAttempt {
      def fahrenheitToCelsius(f: Double): Double =
        (f - 32) * 5.0 / 9.0

      def converter(): Unit = {
        println("Enter a temperature in degrees Fahrenheit: ")
        val d = StdIn.readLine.toDouble
        println(fahrenheitToCelsius(d))
      }

      // Unfortunately, we run into problems if we want to make converter into a pure function that returns an IO:
      def converterIO: IO = {
        val prompt: IO = PrintLine("Enter a temperature in degrees Fahrenheit: ")
        // now what ???
        ???
      }

      // In Scala, readLine is a def with the side effect of capturing a line of input from the console.
      // It returns a String.
      // We could wrap a call to readLine in IO, but we have nowhere to put the result!
      // We don’t yet have a way of representing this sort of effect.
      // The problem is that our current IO type can’t express computations that yield a value of some meaningful type — our interpreter of IO just produces Unit as its output.
      // We extend our IO type to allow input, by adding a type parameter as shown below as
      // sealed trait IO[A]
    }
  }

  "Functional program that converts Fahrenheit to Celsius" should {
    "use monadic IO" in {
      // See App below

      import FunctionalProgramThatConvertsFahrenheitToCelsiusUsingMonadicIO._

      // And because IO forms a Monad, we can use all the monadic combinators e.g.

      // An IO[Unit] that reads a line from the console and echoes it back
      val echo = ReadLine.flatMap(PrintLine)

      // An IO[Int] that parses an Int by reading a line from the console
      val readInt = ReadLine.map(_.toInt)
    }
  }
}

object FunctionalProgramThatConvertsFahrenheitToCelsiusUsingMonadicIO extends App {
  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0

  def ReadLine: IO[String] = IO { StdIn.readLine }

  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  // Our converter definition no longer has side effects — it’s a referentially transparent description of a computation with effects,
  // and converter.run is the interpreter that will actually execute those effects:
  converter.run
}

// In this case IO forms a Monad
sealed trait IO[A] { self =>
  def run: A

  def map[B](f: A => B): IO[B] =
    new IO[B] {
      def run: B = f(self.run)
    }

  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO[B] {
      def run: B = f(self.run).run
    }
}

object IO extends Monad[IO] {
  def apply[A](a: => A): IO[A] =
    unit(a)

  def unit[A](a: => A): IO[A] =
    new IO[A] {
      def run: A = a
    }

  override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] =
    fa flatMap f
}