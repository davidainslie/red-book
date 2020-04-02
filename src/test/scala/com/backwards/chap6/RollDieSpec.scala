package com.backwards.chap6

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.chap6.RNG._

class RollDieSpec extends AnyWordSpec with Matchers {
  "Roll die" should {
    "be deterministic with a functional API - first attempt can have an off by 1 issue i.e. the die range is 0 to 5" in {
      def rollDie: Rand[Int] = nonNegativeLessThan(6)

      val (zero, _) = rollDie(SimpleRNG(5))

      zero mustBe 0
    }

    "be deterministic with a functional API - second attempt fixes off by 1 error" in {
      def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

      val (n, _) = rollDie(SimpleRNG(5))

      n mustBe 1
    }
  }
}