package com.backwards.chap10

import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import com.backwards.chap10.Monoid._

/**
 * A monoid consists of the following:
 * - Some type A
 * - An associative binary operation, op, that takes two values of type A and combines them into one:
 *   op(op(x, y), z) == op(x, op(y, z)) for any choice of x: A, y: A, z: A
 * - A value, zero: A, that is an identity for that operation:
 *   op(x, zero) == x and op(zero, x) == x for any x: A
 *
 * Definition of Monoid:
 * Type A forms a monoid under the operations defined by the Monoid[A] instance.
 */
class Chap10Spec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  "Monoid" should {
    "10.1 primitive monoids" in {
      intAddition.op(1, 2) mustBe 3
      intMultiplication.op(1, 2) mustBe 2
      booleanOr.op(false, true) mustBe true
      booleanAnd.op(false, true) mustBe false
    }

    "10.2 option" in {
      implicit val intMonoid: Monoid[Int] = intAddition

      optionMonoid[Int].op(Some(1), Some(2)) mustBe Some(3)
    }

    "10.3 endofunction - A function having the same argument and return type is sometimes called an endofunction" in {
      val f: Int => Int = endoMonoid.op((i: Int) => i + 1, (i: Int) => i + 2)

      f(5) mustBe 8
    }

    "10.4 monoid laws" in {
      def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Assertion =
        forAll(gen, gen, gen) { (a1, a2, a3) =>
          m.op(a1, m.op(a2, a3)) mustBe m.op(m.op(a1, a2), a3)
          m.op(a1, m.zero) mustBe a1
        }

      monoidLaws(intAddition, Gen.chooseNum(-100, 100))
    }
  }
}

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {
  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2

    val zero = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    val zero: List[Nothing] = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2

    def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2

    def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    def zero: Boolean = true
  }

  def optionMonoid[A: Monoid]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = (a1, a2) match {
      case (Some(a1), Some(a2)) => Option(implicitly[Monoid[A]].op(a1, a2))
      case _ => None
    }

    def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A =
      a1 andThen a2

    def zero: A => A = identity
  }
}