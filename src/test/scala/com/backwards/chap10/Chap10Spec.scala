package com.backwards.chap10

import java.util.concurrent.{ExecutorService, Executors, Future, TimeUnit}
import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import com.backwards.chap10.Monoid._
import com.backwards.chap7.Par
import com.backwards.chap7.Par._

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

    "10.5 foldMap" in {
      foldMap(List("1", "2", "3"), intAddition)(_.toInt) mustBe 6
    }

    "10.6 foldLeft and foldRight in terms of foldMap" in {
      foldLeft(List(1, 2, 3))(intAddition.zero)(intAddition.op) mustBe 6

      foldRight(List(1, 2, 3))(intAddition.zero)(intAddition.op) mustBe 6
    }

    /*
    Balanced fold:

    As an example, suppose we have a sequence a, b, c, d
    that we’d like to reduce using some monoid.

    Folding to the right, the combination of a, b, c, and d would look like this:
      op(a, op(b, op(c, d)))

    Folding to the left would look like this:
      op(op(op(a, b), c), d)

    But a balanced fold looks like this where the balanced fold allows for parallelism:
      op(op(a, b), op(c, d))
    */

    "10.7 foldMapV for IndexedSeq which allows splitting strategy recursively processing each half and then adding the answers together with the monoid" in {
      foldMapV(Vector("1", "2", "3"), intAddition)(_.toInt) mustBe 6
    }

    "10.8 parFoldMap" ignore { // TODO - Never finishes !!!
      val executorService: ExecutorService = Executors.newFixedThreadPool(3)

      val par: Par[Int] = parFoldMap(Vector("1", "2", "3"), intAddition)(_.toInt)

      val future: Future[Int] = Par.run(executorService)(par)

      future.get(3, TimeUnit.SECONDS) mustBe 6
    }

    "10.9 ordered" in {
      ordered(Vector(1, 2, 3)) mustBe true

      ordered(Vector(1, 22, 3)) mustBe false
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

  // Example usage:
  val words = List("Hic", "Est", "Index")
  words.foldRight(stringMonoid.zero)(stringMonoid.op)

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

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero) { (b, a) =>
      m.op(b, f(a))
    }

  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)

    val zero: A = m.zero
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v.length match {
    case 0 =>
      m.zero

    case 1 =>
      f(v(0))

    case length =>
      val (l, r) = v.splitAt(length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)

    def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(v)(f).flatMap { bs =>
      foldMapV(bs, par(m))(b => Par.lazyUnit(b))
    }

  // This implementation detects only ascending order,
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    // Our monoid tracks the minimum and maximum element seen so far as well as whether the elements are so far ordered.
    val monoid: Monoid[Option[(Int, Int, Boolean)]] = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] =
        (o1, o2) match {
          // The ranges should not overlap if the sequence is ordered.
          case (Some((x1, y1, p)), Some((x2, y2, q))) => Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
          case (x, None) => x
          case (None, x) => x
        }

      val zero: None.type = None
    }

    // The empty sequence is ordered, and each element by itself is ordered.
    foldMapV(ints, monoid)(i => Some((i, i, true))).forall(_._3)
  }
}