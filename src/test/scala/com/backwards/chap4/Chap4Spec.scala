package com.backwards.chap4

import scala.annotation.tailrec
import scala.math._
import scala.util.chaining._
import cats.implicits._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Chap4Spec extends AnyWordSpec with Matchers {
  "Option" should {
    object Option {
      sealed trait Option[+A] {
        /*
        def map[B](f: A => B): Option[B] =
          flatMap(f andThen Some.apply)

        def flatMap[B](f: A => Option[B]): Option[B] = this match {
          case Some(a) => f(a)
          case None => None
        }
        */

        def map[B](f: A => B): Option[B] = this match {
          case Some(a) => Some(f(a))
          case None => None
        }

        def flatMap[B](f: A => Option[B]): Option[B] =
          map(f) getOrElse None

        def getOrElse[B >: A](default: => B): B = this match {
          case Some(a) => a
          case None => default
        }

        def orElse[B >: A](ob: => Option[B]): Option[B] =
          map(Some.apply) getOrElse ob

        def filter(f: A => Boolean): Option[A] =
          flatMap(a => if (f(a)) this else None)
      }

      final case class Some[+A](get: A) extends Option[A]

      final case object None extends Option[Nothing]

      def apply[A](a: => A): Option[A] =
        try {
          Some(a)
        } catch {
          case _: Exception => None
        }

      /**
       * If the mean of a sequence is m, the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
       * @param seq Seq[Double]
       * @return Option[Double]
       */
      def variance(seq: Seq[Double]): Option[Double] = {
        def mean(seq: Seq[Double]): Option[Double] = seq match {
          case s if s.isEmpty => None
          case s => Some(s.sum / s.length)
        }

        mean(seq).flatMap { m =>
          mean(seq.map(x => pow(x - m, 2)))
        }
      }

      /**
       * One can imagine how any callers of methods that take or return Option will have to be modified to handle either Some or None.
       * But this doesn’t happen, and the reason is that we can lift ordinary functions to become functions that operate on Option.
       * @param f Function A => B
       * @tparam A Type A
       * @tparam B Type B
       * @return Option[A] => Option[B]
       */
      def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

      // For example:
      val absOpt: Option[Double] => Option[Double] = lift(math.abs)

      def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
        a.flatMap(a => b.map(b => f(a, b)))

      def map2UsingForComprehension[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
        for {
          aa <- a
          bb <- b
        } yield f(aa, bb)

      def sequence[A](as: List[Option[A]]): Option[List[A]] = {
        @tailrec
        def sequence(as: List[Option[A]], acc: List[A]): Option[List[A]] = as match {
          case Nil => if (acc == Nil) None else Some(acc)
          case Some(a) +: rest => sequence(rest, acc :+ a)
          case _ => None
        }

        sequence(as, Nil)
      }

      def sequenceAlternative[A](as: List[Option[A]]): Option[List[A]] =
        as.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))

      def sequenceUsingTraverse[A](as: List[Option[A]]): Option[List[A]] =
        traverse(as)(identity)

      def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
        @tailrec
        def traverse(as: List[A], acc: List[B]): Option[List[B]] = as match {
          case Nil =>
            if (acc == Nil) None else Some(acc)

          case a +: rest =>
            f(a) match {
              case Some(a) => traverse(rest, acc :+ a)
              case _ => if (acc == Nil) None else Some(acc)
            }

          case _ =>
            None
        }

        traverse(as, Nil)
      }

      def traverseAlternative[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
        as match {
          case Nil => Some(Nil)
          case h :: t => map2(f(h), traverseAlternative(t)(f))(_ :: _)
        }

      def traverseAlternativeAlternative[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
        as.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))

    }

    "4.1 map" in {
      import Option._

      Some(1).map(_ + 1 pipe(_.toString)) mustBe Some("2")
      (None: Option[Int]).map(_ + 1 pipe(_.toString)) mustBe None
    }

    "4.1 flatMap" in {
      import Option._

      Some(1).flatMap(_ + 1 pipe(_.toString) pipe Some.apply) mustBe Some("2")
      (None: Option[Int]).flatMap(_ + 1 pipe(_.toString) pipe Some.apply) mustBe None
    }

    "4.1 getOrElse" in {
      import Option._

      Some(1).getOrElse(2) mustBe 1
      (None: Option[Int]).getOrElse(2) mustBe 2
    }

    "4.1 orElse" in {
      import Option._

      Some(1).orElse(Some(5)) mustBe Some(1)
      (None: Option[Int]).orElse(Some(5)) mustBe Some(5)
    }

    "4.1 filter" in {
      import Option._

      Some(1).filter(_ > 0) mustBe Some(1)
      Some(1).filter(_ > 10) mustBe None
      (None: Option[Int]).filter(_ > 0) mustBe None
    }

    "4.2 function variance" in {
      import Option._

      variance(List(5, 6.7, 8.1)).map(d => d mustBe 1.6067 +- 0.0001)
    }

    "4.3 map2 - combines two Option values using a binary function - if either Option value is None, then the return value is None" in {
      import Option._

      // Let's say we have (ordinary) function:
      def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double =
        age * numberOfSpeedingTickets

      // And let's suppose the data originally comes in from some web form of just Strings:
      def parseInsuranceRateQuote(
        age: String,
        numberOfSpeedingTickets: String
      ): Option[Double] = {
        val optAge: Option[Int] = Option(age.toInt)
        val optTickets: Option[Int] = Option(numberOfSpeedingTickets.toInt)

        // We need a higher order function that lifts
        map2(optAge, optTickets)(insuranceRateQuote)
      }

      parseInsuranceRateQuote("21", "2") mustBe Some(42)

      map2(Some(1), Some(5))(_ + _) mustBe Some(6)
      map2(None: Option[Int], Some(5))(_ + _) mustBe None
      map2(Some(1), None: Option[Int])(_ + _) mustBe None
    }

    """4.4 sequence - combines a list of Options into one Option containing a list of all the Some values in the original list.
       If the original list contains None even once, the result of the function should be None; otherwise the result should be Some with a list of all the values.""" in {
      import Option._

      sequence(List(Some(1), Some(5), Some(10))) mustBe Some(List(1, 5, 10))
      sequence(List(Some(1), None, Some(10))) mustBe None
      sequence(Nil) mustBe None
    }

    """4.5 - traverse""" in {
      import Option._

      // Sometimes we’ll want to map over a list using a function that might fail, returning None if applying it to any element of the list returns None.
      // For example, what if we have a whole list of String values that we wish to parse to Option[Int]? In that case, we can simply sequence the results of the map:

      def parseInts(as: List[String]): Option[List[Int]] =
        sequence(as map (i => Option(i.toInt)))

      // Unfortunately, this is inefficient, since it traverses the list twice;
      // first to convert each String to an Option[Int];
      // and a second pass to combine these Option[Int] values into an Option[List[Int]].
      // Wanting to sequence the results of a map this way is a common enough occurrence to warrant a new generic function, traverse:

      traverse(List("1", "5", "10"))(s => Option(s.toInt)) mustBe Some(List(1, 5, 10))
    }
  }

  "Either" should {
    object Either {
      sealed trait Either[+E, +A] {
        def map[B](f: A => B): E Either B =
          flatMap(a => Right(f(a)))

        def flatMap[EE >: E, B](f: A => EE Either B): EE Either B = this match {
          case l @ Left(_) => l
          case Right(a) => f(a)
        }

        def orElse[EE >: E, B >: A](b: => EE Either B): EE Either B = this match {
          case r @ Right(_) => r
          case _ => b
        }

        def map2[EE >: E, B, C](b: EE Either B)(f: (A, B) => C): EE Either C = {
          flatMap(a => b.map(b => f(a, b)))
        }
      }

      case class Left[+E](value: E) extends (E Either Nothing)

      case class Right[+A](value: A) extends (Nothing Either A)

      def apply[A](a: => A): Exception Either A =
        try {
          Right(a)
        } catch { case e: Exception =>
          Left(e)
        }
    }

    "4.6a - map" in {
      import Either._

      Right("ye").map(_.toUpperCase) mustBe Right("YE")

      val left = Left(new Exception("Left")): Exception Either String

      left.map(_.toUpperCase) mustBe left
    }

    "4.6b - flatMap" in {
      import Either._

      Right("ye").flatMap(s => Either(s.toUpperCase)) mustBe Right("YE")

      val left = Left(new Exception("Left")): Exception Either String

      left.flatMap(s => Either(s.toUpperCase)) mustBe left
    }

    "4.6c - orElse" in {
      import Either._

      Right("ye").orElse(Right("right")) mustBe Right("ye")

      val left = Left(new Exception("Left")): Exception Either String

      left.orElse(Right("right")) mustBe Right("right")
    }

    "4.6d - map2" in {
      import Either._

      Right("ye").map2(Right("no"))(_ + _) mustBe Right("yeno")

      val left = Left(new Exception("Left")): Exception Either String

      Right("ye").map2(left)(_ + _) mustBe left

      left.map2(Right("no"))(_ + _) mustBe left

      left.map2(left)(_ + _) mustBe left
    }
  }
}