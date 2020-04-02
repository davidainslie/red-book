package com.backwards.chap6

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StateSpec extends AnyWordSpec with Matchers {
  "State as a generalisation of the Rand type defined in Chap6Spc" should {
    "show that one of the Rand functions can be generalised e.g. map" in {
      import com.backwards.chap6.RNG._

      def map[S, A, B](a: S => (A, S))(f: A => B): S => (B, S) = ???

      // Remembering that the specific version for Rand is:
      def mapOriginal[A, B](s: Rand[A])(f: A => B): Rand[B] = ???
    }

    "be defined as a type alias" in {
      type State[S, +A] = S => (A, S)

      // Remembering that the specific declaration is:
      type Rand[+A] = RNG => (A, RNG)
    }

    "allow Rand to be aliased using the State alias" in {
      type State[S, +A] = S => (A, S)

      type Rand[+A] = State[RNG, A]
    }

    "be defined as its own ADT instead of just a type alias" in {
      case class State[S, +A](run: S => (A, S))
    }
  }

  "State" should {
    import com.backwards.chap6.State._

    // In the following examples, we effectively have a "counter" as the state.

    "6.10" in {
      val state = State[Int, String](i => ("hi", i))

      state.run(10) mustBe ("hi", 10)
    }

    "6.10a unit" in {
      val state = unit[Int, String]("hi")

      state.run(10) mustBe ("hi", 10)
    }

    "6.10b map (first attempt)" in {
      val state = unit[Int, String]("hi")

      state.mapFirstAttempt(s => s"$s there").run(10) mustBe ("hi there", 10)
    }

    "6.10c flatMap" in {
      val state = unit[Int, String]("hi")

      state.flatMap(s => unit(s"$s there")).run(10) mustBe ("hi there", 10)
    }

    "6.10d map (second attempt using flatMap)" in {
      val state = unit[Int, String]("hi")

      state.map(s => s"$s there").run(10) mustBe ("hi there", 10)
    }

    "6.10e map2" in {
      val state1 = unit[Int, String]("hi")

      val state2 = unit[Int, Boolean](true)

      val newState = state1.map2(state2) { (s, b) =>
        if (b) s"$s there"
        else s
      }

      newState.run(10) mustBe ("hi there", 10)
    }

    "6.10f sequence" in {
      val state = unit[Int, String]("hi")

      sequence(List.fill(3)(state)).run(10) mustBe (List.fill(3)("hi"), 10)
    }

    "6.11 state machine" in {
      sealed trait Input

      case object Coin extends Input

      case object Turn extends Input

      case class Machine(locked: Boolean, candies: Int, coins: Int)

      def update: Input => Machine => Machine =
        input => machine =>
          (input, machine) match {
            case (_, m @ Machine(_, 0, _)) => m
            case (Coin, m @ Machine(false, _, _)) => m
            case (Turn, m @ Machine(true, _, _)) => m
            case (Coin, Machine(true, candy, coin)) => Machine(locked = false, candy, coin + 1)
            case (Turn, Machine(false, candy, coin)) => Machine(locked = true, candy - 1, coin)
          }

      def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
        _ <- sequence(inputs map (update andThen modify[Machine]))
        s <- get
      } yield (s.coins, s.candies)

      // If the input Machine has 10 coins and 5 candies, and a total of 4 candies are suc- cessfully bought, the output should be (14, 1)
      val ((coins, candies), machine) =
        simulateMachine(List(Coin, Coin, Turn, Turn)).run(Machine(locked = true, candies = 5, coins = 10))

      println(s"Coins = $coins, Candies = $candies")
    }
  }
}