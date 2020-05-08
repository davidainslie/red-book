package com.backwards.chap10

import java.util.concurrent.{ExecutorService, Executors, Future, TimeUnit}
import scala.annotation.tailrec
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

    "10.10 word count" in {
      sealed trait WC

      case class Stub(chars: String) extends WC

      case class Part(lStub: String, words: Int, rStub: String) extends WC

      val wcMonoid: Monoid[WC] = new Monoid[WC] {
        def op(a1: WC, a2: WC): WC = (a1, a2) match {
          case (Stub(c1), Stub(c2)) =>
            Stub(c1 + c2)

          case (Part(lStub1, words1, rStub1), Part(lStub2, words2, rStub2)) =>
            Part(lStub1: String, words1 + (if ((rStub1 + lStub2).isEmpty) 0 else 1) + words2, rStub2)

          case (Stub(c1), Part(lStub2, words2, rStub2)) =>
            Part(c1 + lStub2, words2, rStub2)

          case (Part(lStub1, words1, rStub1), Stub(c2)) =>
            Part(lStub1, words1, rStub1 + c2)
        }

        def zero: WC = Stub("")
      }

      def count(s: String): Int = {
        // A single character's count. Whitespace does not count, and non-whitespace starts a new Stub.
        def wc(c: Char): WC =
          if (c.isWhitespace)
            Part("", 0, "")
          else
            Stub(c.toString)

        // `unstub(s)` is 0 if `s` is empty, otherwise 1.
        def unstub(s: String): Int = s.length min 1

        foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
          case Stub(s) => unstub(s)
          case Part(l, w, r) => unstub(l) + w + unstub(r)
        }
      }

      count("Well howdy do") mustEqual 3
    }

    // 10.11 to 10.15 in Foldable

    "10.16 product" in {
      val m: Monoid[(Int, String)] = productMonoid(intAddition, stringMonoid)

      val (int, string) = m.op(1 -> "1", 2 -> "2")

      int mustBe 3
      string mustBe "12"
    }

    "10.17 function" in {
      val m: Monoid[String => Int] = functionMonoid[String, Int](intAddition)

      val f: String => Int = m.op(_.length, _.indexOf("z"))

      f("x") mustBe 0
    }

    "10.18 bag" in {
      bag(Vector("a", "rose", "is", "a", "rose")) mustBe Map("a" -> 2, "rose" -> 2, "is" -> 1)
    }

    "extra - perform multiple calculations simultaneously when folding over a data structure e.g. calculate mean by simultaneously calculating length and sum of given list" in {
      val m: Monoid[(Int, Int)] = productMonoid(intAddition, intAddition)

      val (length, sum) = ListFoldable.foldMap(List(1, 2, 3, 4))(a => (1, a))(m)

      val mean: Double = sum / length.toDouble

      mean mustBe 2.5
    }
  }

  "Foldable" should {
    "10.12a Foldable[List]" in {
      import ListFoldable._

      foldRight(List(1, 2, 3))(0)(_ + _) mustBe 6

      foldLeft(List(1, 2, 3))(0)(_ + _) mustBe 6

      foldMap(List(1, 2, 3))(identity)(Monoid.intAddition) mustBe 6
    }

    "10.12b Foldable[IndexedSeq]" in {
      import IndexedSeqFoldable._

      foldRight(Vector(1, 2, 3))(0)(_ + _) mustBe 6

      foldLeft(Vector(1, 2, 3))(0)(_ + _) mustBe 6

      foldMap(Vector(1, 2, 3))(identity)(Monoid.intAddition) mustBe 6
    }

    "10.12c Foldable[LazyList]" in {
      import LazyListFoldable._

      foldRight(LazyList(1, 2, 3))(0)(_ + _) mustBe 6

      foldLeft(LazyList(1, 2, 3))(0)(_ + _) mustBe 6

      foldMap(LazyList(1, 2, 3))(identity)(Monoid.intAddition) mustBe 6
    }

    "10.13 tree" in {
      import TreeFoldable._

      foldRight(Branch(
        Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))
      ))(0)(_ + _) mustBe 15

      foldLeft(Branch(
        Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))
      ))(0)(_ + _) mustBe 15

      foldMap(Branch(
        Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))
      ))(identity)(Monoid.intAddition) mustBe 15
    }

    "10.14 option" in {
      import OptionFoldable._

      foldRight(Some(5))("6")((i, s) => s + i) mustBe "65"

      foldLeft(Some(5))("6")((s, i) => s + i) mustBe "65"

      foldMap(Some(5))("6" + _)(Monoid.stringMonoid) mustBe "65"
    }

    "10.15 toList" in {
      import OptionFoldable._

      toList(Some(5)) mustBe List(5)
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

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(a1: (A, B), a2: (A, B)): (A, B) = {
      val (a1A, a1B) = a1
      val (a2A, a2B) = a2

      (A.op(a1A, a2A), B.op(a1B, a2B))
    }

    def zero: (A, B) = (A.zero, B.zero)
  }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(a1: A => B, a2: A => B): A => B =
      a => B.op(a1(a), a2(a))

    def zero: A => B = _ => B.zero
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    IndexedSeqFoldable.foldLeft(as)(Map.empty[A, Int]) { (m, a) =>
      m.updatedWith(a)(_.map(_ + 1).orElse(Option(1)))
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map.empty[K, V]

      def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
        }
    }

  def bagUsingMapMergeMonoid[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))((a: A) => Map(a -> 1))
}

/**
 * Here we’re abstracting over a type constructor F.
 * We write it as F[_], where the underscore indicates that F is not a type but a type constructor that takes one type argument.
 * Just like functions that take other functions as arguments are called higher-order functions,
 * something like Foldable is a `higher-order type constructor` or a `higher-kinded type`.
 *
 * Just like values and functions have types, types and type constructors have kinds.
 * Scala uses kinds to track how many type arguments a type constructor takes, whether it’s co- or contravariant in those arguments,
 * and what the kinds of those arguments are.
 */
trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List.empty[A])(_ :: _)

  def flip[A, B](f: (A, B) => B): (B, A) => B =
    (b: B, a: A) => f(a, b)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldLeft(as)(z)(flip(f))

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    @tailrec
    def go(as: List[A], acc: B): B = as match {
      case Nil => acc
      case a +: as => go(as, f(acc, a))
    }

    go(as, z)
  }

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(f(a), b))

  override def toList[A](as: List[A]): List[A] = as
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    foldLeft(as)(z)(flip(f))

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = {
    @tailrec
    def go(as: IndexedSeq[A], acc: B): B = as match {
      case as if as.isEmpty => acc
      case a +: as => go(as, f(acc, a))
    }

    go(as, z)
  }

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(f(a), b))
}

object LazyListFoldable extends Foldable[LazyList] {
  override def foldRight[A, B](as: LazyList[A])(z: B)(f: (A, B) => B): B =
    foldLeft(as)(z)(flip(f))

  override def foldLeft[A, B](as: LazyList[A])(z: B)(f: (B, A) => B): B = {
    @tailrec
    def go(as: LazyList[A], acc: B): B = as match {
      case as if as.isEmpty => acc
      case a +: as => go(as, f(acc, a))
    }

    go(as, z)
  }

  override def foldMap[A, B](as: LazyList[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(f(a), b))
}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
    foldLeft(as)(z)(flip(f))

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = {
    def go(as: Tree[A], acc: B): B = as match {
      case Leaf(a) => f(acc, a)
      case Branch(l, r) => go(l, go(r, acc))
    }

    go(as, z)
  }

  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(f(a), b))
}

object OptionFoldable extends Foldable[Option] {
  override def foldRight[A, B](a: Option[A])(z: B)(f: (A, B) => B): B =
    foldLeft(a)(z)(flip(f))

  override def foldLeft[A, B](a: Option[A])(z: B)(f: (B, A) => B): B =
    a.fold(z)(a => f(z, a))

  override def foldMap[A, B](a: Option[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(a)(mb.zero)((b, a) => mb.op(f(a), b))
}