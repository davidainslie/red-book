package com.backwards.chap6

import scala.annotation.tailrec
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.chap6.RNG._

class Chap6Spec extends AnyWordSpec with Matchers {
  "RNG" should {
    "6.1 nonNegativeInt - a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue (inclusive)" in {
      val (n, rng2) = nonNegativeInt(SimpleRNG(42))

      n mustBe 16159453
      rng2 mustBe SimpleRNG(1059025964525L)
    }

    "6.2 double" in {
      val (n, rng2) = double(SimpleRNG(42))

      n mustBe 0.007524831686168909
      rng2 mustBe SimpleRNG(1059025964525L)
    }

    "6.3a intDouble" in {
      val ((i, d), rng3) = intDouble(SimpleRNG(42))

      i mustBe 16159453
      d mustBe 0.5967354848980904
      rng3 mustBe SimpleRNG(197491923327988L)
    }

    "6.3b doubleInt" in {
      val ((d, i), rng3) = doubleInt(SimpleRNG(42))

      i mustBe 16159453
      d mustBe 0.5967354848980904
      rng3 mustBe SimpleRNG(197491923327988L)
    }

    "6.3b double3" in {
      val ((d1, d2, d3), rng4) = double3(SimpleRNG(42))

      d1 mustBe 0.007524831686168909
      d2 mustBe 0.5967354848980904
      d3 mustBe 0.15846728393808007
      rng4 mustBe SimpleRNG(259172689157871L)
    }

    "6.4 ints - generates a list of random integers" in {
      val (ns, _) = ints(5)(SimpleRNG(42))

      val (nsAlternative, _) = intsAlternative(5)(SimpleRNG(42))

      ns mustBe nsAlternative
    }

    "6.5 double using map" in {
      val (n, rng2) = doubleUsingMap(SimpleRNG(42))

      n mustBe 0.007524831686168909
      rng2 mustBe SimpleRNG(1059025964525L)
    }

    """6.6 map2
      map isn’t powerful enough to implement intDouble and doubleInt.
      We need is a new combinator map2 that can combine two RNG actions into one using a binary rather than unary function.
    """ in {
      val rng: Rand[Int] = map2(nonNegativeInt, nonNegativeInt)(_ + _)

      val (n, _) = rng(SimpleRNG(42))

      val (na, rngA) = nonNegativeInt(SimpleRNG(42))
      val (nb, _) = nonNegativeInt(rngA)

      n mustBe (na + nb)
    }

    "6.7 sequence - combine list of transiations into a single transition" in {
      val r: Rand[List[Double]] = sequence(List.fill(3)(doubleUsingMap))

      val (ns, _) = r(SimpleRNG(42))

      val (n1, rng2) = doubleUsingMap(SimpleRNG(42))
      val (n2, rng3) = doubleUsingMap(rng2)
      val (n3, _) = doubleUsingMap(rng3)

      ns mustBe List(n1, n2, n3)
    }

    "6.8a flatMap" in {
      val r: Rand[Int] = flatMap(nonNegativeInt)(a => unit(a + 1))

      val (n, rng2) = r(SimpleRNG(42))

      n mustBe (16159453 + 1)
      rng2 mustBe SimpleRNG(1059025964525L)
    }

    "6.8b nonNegativeLessThan" in {
      val r: Rand[Int] = nonNegativeLessThan(10)

      val (n, _) = r(SimpleRNG(42))

      n must be <= 10
    }

    "6.9a map using flatMap" in {
      val (n, rng2) = mapUsingFlatMap(double)(_ * 2)(SimpleRNG(42))

      n mustBe 0.007524831686168909 * 2
      rng2 mustBe SimpleRNG(1059025964525L)
    }

    "6.9b map2 using flatMap" in {
      val rng = map2UsingFlatMap(nonNegativeInt, nonNegativeInt)(_ + _)

      val (n, _) = rng(SimpleRNG(42))

      val (na, rngA) = nonNegativeInt(SimpleRNG(42))
      val (nb, _) = nonNegativeInt(rngA)

      n mustBe (na + nb)
    }
  }
}

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)

      val n = (newSeed >>> 16).toInt

      (n, nextRNG)
    }
  }

  // Books solution gave this comment:
  // We need to be quite careful not to skew the generator.
  // Since `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)`, it suffices to increment the negative numbers by 1 and make them positive.
  // This maps Int.MinValue to Int.MaxValue and -1 to 0.
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt

    (if (n < 0) -(n + 1) else n, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = nonNegativeInt(rng)

    (n / (Int.MaxValue.toDouble + 1), rng2)
  }

  val doubleUsingMap: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)

    (i -> d, rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    intDouble(rng) match {
      case ((i, d), rng3) => (d -> i, rng3)
    }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)

    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    (0 until count).foldLeft((List.empty[Int], rng)) { case ((ints, rng), _) =>
      val (n, nextRng) = rng.nextInt
      (ints :+ n, nextRng)
    }

  def intsUsingSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  // A simple recursive solution
  def intsAlternative(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count == 0) {
      (List(), rng)
    } else {
      val (x, r1)  = rng.nextInt
      val (xs, r2) = intsAlternative(count - 1)(r1)
      (x :: xs, r2)
    }

  // A tail-recursive solution
  def intsAlternativeAgain(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      if (count == 0) {
        (xs, r)
      } else {
        val (x, r2) = r.nextInt
        go(count - 1, r2, x :: xs)
      }

    go(count, rng, List())
  }

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match {
      case (i, rng2) => (i % 2 == 0, rng2)
    }

  // Previously (above) notice a common pattern: each of the functions has a type of the form RNG => (A, RNG) for some type A.
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  // A simple RNG transition is the "unit" action, which passes the RNG state through without using it, always returning a constant value rather than a random value
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  // Map for transforming the output of a state action without modifying the state itself
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = { rng =>
    val (a, rng2) = s(rng)

    (f(a), rng2)
  }

  def mapUsingFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)

    (f(a, b), rng3)
  }

  def map2UsingFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      map(rb)(b => f(a, b))
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)(_ -> _)

  val randIntDouble: Rand[(Int, Double)] =
    both(int, doubleUsingMap)

  val randDoubleInt: Rand[(Double, Int)] =
    both(doubleUsingMap, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    @tailrec
    def sequence(fs: List[Rand[A]], acc: Rand[List[A]]): Rand[List[A]] = fs match {
      case Nil =>
        acc
      case a +: rest =>
        sequence(rest, map2(a, acc)(_ +: _))
    }

    sequence(fs, unit(Nil))
  }

  def sequenceAlternative[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A])) { (f, acc) =>
      map2(f, acc)(_ +: _)
    }

  def nonNegativeLessThanFirstAttempt(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)

    val mod = i % n

    if (i + (n - 1) - mod >= 0)
      (mod, rng2)
    else
      nonNegativeLessThanFirstAttempt(n)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n

      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }
}