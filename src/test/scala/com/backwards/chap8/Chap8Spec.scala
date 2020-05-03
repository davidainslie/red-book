package com.backwards.chap8

import java.util.concurrent.{ExecutorService, Executors}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.chap6.{RNG, State}
import com.backwards.chap6.RNG.SimpleRNG
import com.backwards.chap7.Par
import com.backwards.chap7.Par.Par
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

    "8.13 listOf1 - essentially a non empty list i.e. a list with at least 1 item" in {
      SProp.run(SGen.maxProp)
    }

    "8.14 sorted prop" in {
      SProp.run(SGen.sortedProp)
    }

    "8.15 check" in {
      val ES: ExecutorService = Executors.newCachedThreadPool

      val p2: SProp = SProp.check {
        val p = Par.map(Par.unit(1))(_ + 1)
        val p2 = Par.unit(2)

        p(ES).get == p2(ES).get
      }

      val result: Result = p2.run(100, 100, SimpleRNG(3))
      result mustBe Passed
    }

    "8.15 check nicer" in {
      val ES: ExecutorService = Executors.newCachedThreadPool

      def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
        Par.map2(p, p2)(_ == _)

      val p3 = SProp.check {
        equal(
          Par.map(Par.unit(1))(_ + 1), Par.unit(2)
        )(ES).get
      }

      val result: Result = p3.run(100, 100, SimpleRNG(3))
      result mustBe Passed
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

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))
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

case object Proved extends Result {
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

object SGen {
  import SProp._

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n max 1))

  val smallInt: Gen[Int] = Gen.choose(-10, 10)

  val maxProp: SProp = forAll(listOf1(smallInt)(10)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  // We specify that every sorted list is either empty, has one element,
  // or has no two consecutive elements `(a, b)` such that `a` is greater than `b`.
  val sortedProp: SProp = forAll(Gen.listOf(smallInt)) { l =>
    val ls = l.sorted
    l.isEmpty || ls.tail.isEmpty || !ls.zip(ls.tail).exists { case (a, b) => a > b }
  }
}

import SProp._

case class SProp(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: SProp): SProp = SProp {
    (max,n, rng) => run(max, n, rng) match {
      case Passed | Proved => p.run(max, n, rng)
      case x => x
    }
  }

  def ||(p: SProp): SProp = SProp {
    (max, n, rng) => run(max, n,  rng) match {
      // In case of failure, run the other prop.
      case Falsified(msg, _) => p.tag(msg).run(max,n,rng)
      case x => x
    }
  }

  /* This is rather simplistic - in the event of failure, we simply prepend
   * the given message on a newline in front of the existing message.
   */
  def tag(msg: String): SProp = SProp {
    (max, n, rng) => run(max, n, rng) match {
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

  def run(
    p: SProp,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = SimpleRNG(System.currentTimeMillis)
  ): Unit = p.run(maxSize, testCases, rng) match {
    case Falsified(msg, n) =>
      println(s"! Falsified after $n passed tests:\n $msg")

    case Passed =>
      println(s"+ OK, passed $testCases tests.")

    case Proved =>
      println(s"+ OK, proved property.")
  }

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

  def check(p: => Boolean): SProp = SProp { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val (gen1, probability1) = g1
    val (gen2, probability2) = g2

    // The probability we should pull from `g1`
    val g1Threshold = probability1.abs / (probability1.abs + probability2.abs)

    Gen(State(RNG.double).flatMap(d => if (d < g1Threshold) gen1.sample else gen2.sample))
  }

  // This generator creates a fixed thread pool executor 75% of the time and an unbounded one 25% of the time.
  val S: Gen[ExecutorService] = weighted(
    Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    Gen.unit(Executors.newCachedThreadPool) -> .25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): SProp =
    SProp.forAll(S.map2(g)((_, _))) {
      case (s, a) => f(a)(s).get
    }
}