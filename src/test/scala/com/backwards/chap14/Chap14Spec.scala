package com.backwards.chap14

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Chap14Spec extends AnyWordSpec with Matchers {
  "STRef" should {
    "illustrate idea" in {
      /**
       * This little program allocates two mutable Int cells, swaps their contents, adds one to both, and then reads their new values.
       * But we can’t yet run this program because run is still protected (and we could never actually pass it a value of type Nothing anyway).
       */
      val st: ST[Nothing, (Int, Int)] = for {
        r1 <- STRef[Nothing, Int](1)
        r2 <- STRef[Nothing, Int](1)
        x <- r1.read
        y <- r2.read
        _ <- r1.write(y + 1)
        _ <- r2.write(x + 1)
        a <- r1.read
        b <- r2.read
      } yield (a, b)
    }

    "illustrate idea safely" in {
      val p: RunnableST[(Int, Int)] = new RunnableST[(Int, Int)] {
        def apply[S]: ST[S, (Int, Int)] = for {
          r1 <- STRef(1)
          r2 <- STRef(2)
          x <- r1.read
          y <- r2.read
          _ <- r1.write(y + 1)
          _ <- r2.write(x + 1)
          a <- r1.read
          b <- r2.read
        } yield (a, b)
      }

      val r = ST runST p

      r mustBe (3, 2)
    }
  }
}

/**
 * This new local effects monad, ST, could stand for "state thread"; "state transition"; "state token"; "state tag"
 * It's different from State monad in that its "run" method is protected.
 */
sealed trait ST[S, A] { self =>
  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S, A](a: => A): ST[S, A] = {
    // Cache the value is case "run" is called more than once.
    lazy val memo = a

    new ST[S, A] {
      def run(s: S): (A, S) = (memo, s)
    }
  }

  def runST[A](st: RunnableST[A]): A =
    st[Unit].run(())._1
    // OR explicitly:
    // st.apply[Unit].run(())._1
}

/**
 * Mutable reference
 */
sealed trait STRef[S, A] {
  protected var cell: A

  def read: ST[S, A] = ST(cell)

  def write(a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S): (Unit, S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] =
    ST(
      new STRef[S, A] {
        var cell: A = a
      }
    )
}

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}

sealed abstract class STArray[S, A: Manifest] {
  protected def value: Array[A]

  def size: ST[S, Int] = ST(value.length)

  // Write a value at the give index of the array
  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S): (Unit, S) = {
      value(i) = a
      ((), s)
    }
  }

  // Read the value at the given index of the array
  def read(i: Int): ST[S, A] = ST(value(i))

  // Turn the array into an immutable list
  def freeze: ST[S, List[A]] = ST(value.toList)

  def fill(xs: Map[Int, A]): ST[S, Unit] =
    xs.foldRight(ST[S, Unit](())) {
      case ((k, v), st) => st flatMap (_ => write(k, v))
    }

  def swap(i: Int, j: Int): ST[S, Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(j, x)
  } yield ()
}

object STArray {
  // Construct an array of the given size filled with the value v
  def apply[S,A:Manifest](sz: Int, v: A): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value: Array[A] = Array.fill(sz)(v)
    })

  def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value: Array[A] = xs.toArray
    })
}