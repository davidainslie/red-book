package com.backwards.chap8

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.chap6.{RNG, State}
import com.backwards.chap6.RNG.SimpleRNG
import com.backwards.chap8.Prop._

class Chap8Spec extends AnyWordSpec with Matchers {
  "Property based testing initial thoughts" should {
    "8.3 check" in {
      trait Prop { self =>
        def check: Boolean

        def &&(p: Prop): Prop = new Prop {
          def check: Boolean = self.check && p.check
        }
      }

      val p1 = new Prop {
        def check: Boolean = true
      }

      val p2 = new Prop {
        def check: Boolean = false
      }

      (p1 && p2).check mustBe false

      val p3 = new Prop {
        def check: Boolean = true
      }

      (p1 && p3).check mustBe true
    }
  }

  "Gen (using State)" should {
    "8.4 choose" in {
      val g: Gen[Int] = Gen.choose(4, 9)

      val (i, nextRng) = g.sample.run(SimpleRNG(42))

      i mustBe 7
    }

    "8.5a unit" in {
      val (i, nextRng) = Gen.unit(5).sample.run(SimpleRNG(42))

      i mustBe 5
    }

    "8.5b boolean" in {
      val (t, nextRng) = Gen.boolean.sample.run(SimpleRNG(42))

      t mustBe false

      val (f, _) = Gen.boolean.sample.run(SimpleRNG(43))

      f mustBe true
    }

    "8.5c listOfN" in {
      val (ns, nextRng) = Gen.listOfN(3, Gen.choose(4, 9)).sample.run(SimpleRNG(92))

      ns mustBe List(7, 8, 5)
    }

    "8.6a flatMap" in {
      val (i, nextRng) = Gen.unit(5).flatMap(i => Gen.unit(i + 1)).sample.run(SimpleRNG(42))

      i mustBe 6
    }

    "8.6b listOfN using flatMap" in {
      val (ns, nextRng) = Gen.choose(4, 9).listOfN(Gen.unit(3)).sample.run(SimpleRNG(92))

      ns mustBe List(7, 8, 5)
    }

    "8.7 union" in {
      val (i, nextRng) = Gen.union(Gen.unit(5), Gen.unit(6)).sample.run(SimpleRNG(42))

      i mustBe 6
    }

    "8.8 weighted" in {
      val (i, nextRng) = Gen.weighted(Gen.unit(5) -> 10, Gen.unit(6) -> 1).sample.run(SimpleRNG(42))

      i mustBe 5
    }
  }

  "Prop" should {
    "8.9a &&" in {
      val prop: Prop = Prop.forAll(Gen.unit(5))(_ > 4) && Prop.forAll(Gen.unit(5))(_ > 1)

      val result: Result = prop.run(11, SimpleRNG(42))

      result mustBe Passed


      val failingProp: Prop = Prop.forAll(Gen.unit(5))(_ > 4) && Prop.forAll(Gen.unit(5))(_ > 5)

      val failingResult: Result = failingProp.run(11, SimpleRNG(42))

      failingResult mustBe Falsified(failure = "5", successes = 0)
    }

    "8.9b ||" in {
      val prop: Prop = Prop.forAll(Gen.unit(5))(_ > 4) || Prop.forAll(Gen.unit(5))(_ > 5)

      val result: Result = prop.run(11, SimpleRNG(42))

      result mustBe Passed
    }
  }

  /**
   * Test case minimization comes in two flavours:
   * - Shrinking: After we’ve found a failing test case, we can run a separate procedure to minimize the test case by successively decreasing its “size” until it no longer fails.
   * - Sized generation: Rather than shrinking test cases after the fact, we simply generate our test cases in order of increasing size and complexity (which we'll do here).
   */
  "Sized Gen (with a new SGen)" should {
    "8.10 unsized - converting a Gen to SGen" in {
      val gen: Gen[Int] = Gen.unit(5)

      val sgen: SGen[Int] = gen.unsized

      val (i, nextRng) = sgen.forSize(1000).sample.run(SimpleRNG(42))

      i mustBe 5
    }

    "8.11a apply - a helper method" in {
      val sgen: SGen[Int] = SGen(_ => Gen.unit(5))

      val (i, nextRng) = sgen.forSize(1000).sample.run(SimpleRNG(42))

      i mustBe 5
    }

    "8.11b map - a helper method" in {
      val sgen: SGen[Int] = SGen(_ => Gen.unit(5))

      val (i, nextRng) = sgen.map(_ + 2).forSize(1000).sample.run(SimpleRNG(42))

      i mustBe 7
    }

    "8.11c flatMap - a helper method" in {
      val sgen: SGen[Int] = SGen(_ => Gen.unit(5))

      val (i, nextRng) = sgen.flatMap(i => SGen(_ => Gen.unit(i))).forSize(1000).sample.run(SimpleRNG(42))

      i mustBe 5
    }

    "8.12 listOf" in {
      val sgen: SGen[List[Int]] = Gen.listOf(Gen.unit(5))

      val (i, nextRng) = sgen.forSize(10).sample.run(SimpleRNG(42))

      i mustBe List.fill(10)(5)
    }
  }
}

final case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap listOfN

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def unsized: SGen[A] =
    SGen(_ => this)
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean flatMap {
      case true => g1
      case false => g2
    }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val (gen1, probability1) = g1
    val (gen2, probability2) = g2

    // The probability we should pull from `g1`
    val g1Threshold = probability1.abs / (probability1.abs + probability2.abs)

    Gen(State(RNG.double).flatMap(d => if (d < g1Threshold) gen1.sample else gen2.sample))
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))
}

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop { (n, rng) =>
    run(n, rng) match {
      case Passed => p.run(n, rng)
      case result => result
    }
  }

  def ||(p: Prop): Prop = Prop { (n, rng) =>
    run(n, rng) match {
      // In case of failure, run the other prop.
      case Falsified(msg, _) => p.tag(msg).run(n, rng)
      case result => result
    }
  }

  // This is rather simplistic - in the event of failure, we simply prepend the given message on a newline in front of the existing message.
  def tag(msg: String): Prop = Prop { (n, rng) =>
    run(n, rng) match {
      case Falsified(e, c) => Falsified(s"$msg\n$e", c)
      case result => result
    }
  }
}

object Prop {
  type TestCases = Int

  type SuccessCount = Int

  type FailedCase = String

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(as)(rng).zip(LazyList.from(0)).take(n).map { case (a, i) =>
      try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception =>
        Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"""Test case: $s
       |generated exception: ${e.getMessage}
       |stack trace:
       |${e.getStackTrace.mkString("\n")}""".stripMargin
}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}

case class SGen[A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] =
    SGen(apply(_).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(n => apply(n).flatMap(a => f(a).forSize(n)))
}

import SProp._

case class SProp(run: (MaxSize,TestCases,RNG) => Result) {
  def &&(p: SProp): SProp = SProp {
    (max,n,rng) => run(max,n,rng) match {
      case Passed /*| Proved*/ => p.run(max, n, rng)
      case x => x
    }
  }

  def ||(p: SProp): SProp = SProp {
    (max,n,rng) => run(max,n,rng) match {
      // In case of failure, run the other prop.
      case Falsified(msg, _) => p.tag(msg).run(max,n,rng)
      case x => x
    }
  }

  /* This is rather simplistic - in the event of failure, we simply prepend
   * the given message on a newline in front of the existing message.
   */
  def tag(msg: String): SProp = SProp {
    (max,n,rng) => run(max,n,rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}

object SProp {
  type MaxSize = Int

  type TestCases = Int

  type SuccessCount = Int

  type FailedCase = String

  def apply(f: (TestCases,RNG) => Result): SProp =
    SProp { (_, n, rng) => f(n,rng) }

  def forAll[A](as: Gen[A])(f: A => Boolean): SProp = SProp {
    (n, rng) => randomStream(as)(rng).zip(LazyList.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): SProp =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): SProp = SProp {
    (max, n, rng) =>
      val casesPerSize = (n - 1) / max + 1

      val props: LazyList[SProp] =
        LazyList.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))

      val prop: SProp =
        props.map(p => SProp { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)

      prop.run(max, n, rng)
  }

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"""Test case: $s
       |generated exception: ${e.getMessage}
       |stack trace:
       |${e.getStackTrace.mkString("\n")}""".stripMargin
}