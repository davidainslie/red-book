package com.backwards.chap4

import scala.annotation.tailrec
import scala.{Either => _}
import cats.implicits._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.chap4.Either._

class Chap4EitherSpec extends AnyWordSpec with Matchers {
  val left: Exception Either String = Left(new Exception)

  "Either" should {
    "4.6a map" in {
      Right("ye").map(_.toUpperCase) mustBe Right("YE")

      left.map(_.toUpperCase) mustBe left
    }

    "4.6b flatMap" in {
      Right("ye").flatMap(s => Either(s.toUpperCase)) mustBe Right("YE")

      left.flatMap(s => Either(s.toUpperCase)) mustBe left
    }

    "4.6c orElse" in {
      Right("ye").orElse(Right("right")) mustBe Right("ye")

      left.orElse(Right("right")) mustBe Right("right")
    }

    "4.6d map2" in {
      Right("ye").map2(Right("no"))(_ + _) mustBe Right("yeno")

      Right("ye").map2(left)(_ + _) mustBe left

      left.map2(Right("no"))(_ + _) mustBe left

      left.map2(left)(_ + _) mustBe left
    }

    "4.7a sequence - return the first error that’s encountered, if there is one" in {
      sequence(List(Right("hi"), Right("there"))) mustBe Right(List("hi", "there"))

      sequence(List(Right("hi"), left, Right("there"))) mustBe left
    }

    "4.7b traverse - return the first error that’s encountered, if there is one" in {
      traverse(List("hi", "there"))(s => Right(s.toUpperCase)) mustBe Right(List("HI", "THERE"))

      traverse(List("hi", "WHOOPS", "there")) { s =>
        if (s == "WHOOPS") left else Right(s.toUpperCase)
      } mustBe left
    }

    "4.8a demo" in {
      case class Person(name: Name, age: Age)

      sealed class Name(val value: String)

      sealed class Age(val value: Int)

      def mkName(name: String): String Either Name =
        if (name == "" || name == null) Left("Name is empty.")
        else Right(new Name(name))

      def mkAge(age: Int): String Either Age =
        if (age < 0) Left("Age is out of range.")
        else Right(new Age(age))

      def mkPerson(name: String, age: Int): String Either Person =
        mkName(name).map2(mkAge(age))(Person)

      val Right(Person(name, age)) = mkPerson("Bob", 27)
      name.value mustBe "Bob"
      age.value mustBe 27

      val Left(error) = mkPerson("", -1)
      error mustBe "Name is empty."
    }

    "4.8b" in {
      // TODO
    }
  }
}

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

  def map2[EE >: E, B, C](b: EE Either B)(f: (A, B) => C): EE Either C =
    flatMap(a => b.map(b => f(a, b)))
}

case class Left[+E](value: E) extends (E Either Nothing)

case class Right[+A](value: A) extends (Nothing Either A)

object Either {
  def apply[A](a: => A): Exception Either A =
    try {
      Right(a)
    } catch { case e: Exception =>
      Left(e)
    }

  def sequence[E, A](xs: List[E Either A]): E Either List[A] = {
    @tailrec
    def sequence(xs: List[E Either A], acc: List[A]): E Either List[A] = xs match {
      case Nil => Right(acc)
      case Right(x) +: rest => sequence(rest, acc :+ x)
      case (l @ Left(_)) +: _ => l
    }

    sequence(xs, Nil)
  }

  def sequenceAlternative[E, A](xs: List[E Either A]): E Either List[A] =
    traverse(xs)(identity)

  def traverse[E, A, B](as: List[A])(f: A => E Either B): E Either List[B] = {
    def traverse(as: List[A], acc: List[B]): E Either List[B] = as match {
      case Nil => Right(acc)
      case a +: rest => f(a).flatMap(b => traverse(rest, acc :+ b))
    }

    traverse(as, Nil)
  }

  def traverseAlternative[E, A, B](as: List[A])(f: A => E Either B): E Either List[B] =
    as match {
      case Nil => Right(Nil)
      case h :: t => (f(h) map2 traverseAlternative(t)(f))(_ :: _)
    }

  def traverseAlternativetAlternative[E, A, B](as: List[A])(f: A => E Either B): E Either List[B] =
    as.foldRight[E Either List[B]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))
}