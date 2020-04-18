package com.backwards.chap8

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Chap8Spec extends AnyWordSpec with Matchers {
  "Property based simple testing" should {
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

  "Property based testing where Gen uses State" should {
    import com.backwards.chap6.{RNG, State}
    import com.backwards.chap6.RNG.SimpleRNG

    case class Gen[A](sample: State[RNG, A])

    object Gen {
      def choose(start: Int, stopExclusive: Int): Gen[Int] =
        Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

      def unit[A](a: => A): Gen[A] =
        Gen(State.unit(a))

      def boolean: Gen[Boolean] =
        Gen(State(RNG.boolean))

      def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
        Gen(State.sequence(List.fill(n)(g.sample)))
    }

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

    }

    "8.6b listOfN" in {

    }

    "8.7 union" in {

    }

    "8.8 weighted" in {

    }
  }
}