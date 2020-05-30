package com.backwards.chap13

import scala.annotation.tailrec
import scala.io.StdIn
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.chap12.Monad
import com.backwards.chap7.Par
import com.backwards.chap7.Par.Par

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

  "Program with side effects - using IO" should {
    "apply first attempt" in new IOVersion1 {
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
    "be for example" in new IOVersion1 {
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
    "use monadic IO" in new IOVersion2 {
      // See App below

      import FunctionalProgramThatConvertsFahrenheitToCelsiusUsingMonadicIO._

      // And because IO forms a Monad, we can use all the monadic combinators e.g.

      // An IO[Unit] that reads a line from the console and echoes it back
      val echo = ReadLine.flatMap(PrintLine)

      // An IO[Int] that parses an Int by reading a line from the console
      val readInt = ReadLine.map(_.toInt)
    }
  }

  "Free Monad" should {
    "13.1a map" in new IOVersion6 {
      // TODO - Lost me again.
    }

    "13.1b flatMap" in {

    }

    "13.1c monad" in {

    }

    "13.2 runTrampoline" in {

    }
  }
}

object FunctionalProgramThatConvertsFahrenheitToCelsiusUsingMonadicIO extends App with IOVersion2 {
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

trait IOVersion1 {
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

trait IOVersion2 {
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
}

trait IOVersion3 {
  sealed trait IO[A] {
    def flatMap[B](f: A => IO[B]): IO[B] =
      FlatMap(this, f)

    def map[B](f: A => B): IO[B] =
      flatMap(f andThen (Return(_)))
  }

  // A pure computation that immediately returns an A without any further steps. When run sees this constructor, it knows the computation has finished.
  case class Return[A](a: A) extends IO[A]

  // A suspension of the computation where resume is a function that takes no arguments, but has some effect and yields a result.
  case class Suspend[A](resume: () => A) extends IO[A]

  // A composition of two steps. Reifies flatMap as a data constructor rather than a function. When run sees this, it should first process the subcomputation sub and then continue with k once sub produces a result.
  case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

  object IO extends Monad[IO] { // Notice that none of these operations DO anything
    def unit[A](a: => A): IO[A] =
      Return(a)

    override def flatMap[A,B](a: IO[A])(f: A => IO[B]): IO[B] =
      a flatMap f

    def suspend[A](a: => IO[A]): IO[A] =
      Suspend(() => ()).flatMap(_ => a)

  }

  @tailrec final def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }
}

/**
 * You don't actually need to have any I/O going on inside an IO.
 * So IO is a bit of a misnomer.
 * It really gets that name from the fact that Suspend can contain a side-effecting function.
 * But what we have is not really a monad for I/O — it’s actually a monad for tail-call elimination!
 *
 * We can use the TailRec data type to add trampolining to any function type A => B by modifying the return type B to TailRec[B] instead.
 * E.g. change a program that uses Int => Int to use Int => TailRec[Int].
 *
 * This is just Kleisli composition.
 * In other words, the trampolined function uses Kleisli composition in the TailRec monad instead of ordinary function composition.
 */
trait IOVersion4 {
  sealed trait TailRec[A] {
    def flatMap[B](f: A => TailRec[B]): TailRec[B] =
      FlatMap(this, f)

    def map[B](f: A => B): TailRec[B] =
      flatMap(f andThen (Return(_)))
  }

  case class Return[A](a: A) extends TailRec[A]

  case class Suspend[A](resume: () => A) extends TailRec[A]

  case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]
}

/**
 * Defining an Async type
 */
trait IOVersion5 {
  sealed trait Async[A] {
    def flatMap[B](f: A => Async[B]): Async[B] =
      FlatMap(this, f)

    def map[B](f: A => B): Async[B] =
      flatMap(f andThen (Return(_)))
  }

  case class Return[A](a: A) extends Async[A]

  case class Suspend[A](resume: Par[A]) extends Async[A]

  case class FlatMap[A, B](sub: Async[A], k: A => Async[B]) extends Async[B]

  /**
   * Note that the resume argument to Suspend is now a Par[A] rather than a () => A (or a Function0[A]).
   * The implementation of run changes accordingly.
   * It now returns a Par[A] rather than an A, and we rely on a separate tail-recursive step function to reassociate the FlatMap constructors:
   */
  @tailrec
  def step[A](async: Async[A]): Async[A] = async match {
    case FlatMap(FlatMap(x, f), g) =>
      step(x flatMap (a => f(a) flatMap g))

    case FlatMap(Return(x), f) =>
      step(f(x))

    case _ =>
      async
  }

  def run[A](async: Async[A]): Par[A] = step(async) match {
    case Return(a) => Par.unit(a)

    case Suspend(r) => Par.flatMap(r)(a => run(a))

    case FlatMap(x, f) => x match {
      case Suspend(r) =>
        Par.flatMap(r)(a => run(f(a)))

      case _ =>
        sys.error("Impossible; `step` eliminates these cases")
    }
  }
}

/**
 * Our Async data type now supports asynchronous computations — we can embed them using the Suspend constructor, which takes an arbitrary Par.
 * This works, but we take this idea one step further and abstract over the choice of type constructor used in Suspend.
 * To do that, we’ll generalize TailRec / Async and parameterize it on some type constructor F rather than use Function0 or Par specifically.
 * We’ll name this more abstract data type Free.
 *
 * What is the meaning of Free[F, A]? Essentially, it’s a recursive structure that contains a value of type A wrapped in zero or more layers of F.
 * It's a monad because flatMap lets us take the A and from it generate more layers of F.
 * Before getting at the result, an interpreter of the structure must be able to process all of those F layers.
 * We can view the structure and its interpreter as coroutines that are interacting, and the type F defines the protocol of this interaction.
 * By choosing our F carefully, we can precisely control what kinds of interactions are allowed.
 *
 * Put another way, it’s a tree with data of type A at the leaves, where the branching is described by F.
 *
 * Put yet another way, it’s an abstract syntax tree for a program in a language whose instructions are given by F, with free variables in A.
 */
trait IOVersion6 {
  // The difference between Free and TailRec is that Free is parameterized with a type constructor F.
  // TailRec is a special case of Free where F is fixed to be Function0.
  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] =
      FlatMap(this, f)

    def map[B](f: A => B): Free[F, B] =
      flatMap(f andThen (Return(_)))
  }

  case class Return[F[_], A](a: A) extends Free[F, A]

  // The suspension is now of some arbitrary type F rather than Function0.
  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]

  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]


  // Then TailRec and Async are simply type aliases:
  type TailRec[A] = Free[Function0, A]

  type Async[A] = Free[Par, A]


  // def freeMonad[F[_]]: Monad[({ type f[a] = Free[F, a] })#f]
  def freeMonad[F[_]]: Monad[Free[F, *]] =
    // new Monad[({ type f[a] = Free[F, a] })#f]
    new Monad[Free[F, *]] {
      def unit[A](a: => A): Free[F, A] =
        Return(a)

      override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
        fa flatMap f
    }

  /**
   * A specialized tail-recursive interpreter, runTrampoline, for running a Free[Function0, A]
   */
  @tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(a) => a

    case Suspend(r) => r()

    case FlatMap(x, f) => x match {
      case Return(a) => runTrampoline(f(a))

      case Suspend(r) => runTrampoline(f(r()))

      case FlatMap(a0, g) => runTrampoline {
        a0 flatMap {
          a0 => g(a0) flatMap f
        }
      }
    }
  }

  def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = step(a) match {
    case Return(a) => F.unit(a)

    case Suspend(r) => r

    case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))

    case _ => sys.error("Impossible, since `step` eliminates these cases")
  }

  @tailrec
  def step[F[_], A](a: Free[F, A]): Free[F, A] = a match {
    case FlatMap(FlatMap(x, f), g) =>
      step(x flatMap (a => f(a) flatMap g))

    case FlatMap(Return(x), f) =>
      step(f(x))

    case _ => a
  }
}