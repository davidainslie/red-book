package com.backwards.chap11

import scala.annotation.tailrec
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.chap6.State

class Chap11Spec extends AnyWordSpec with Matchers {
  "Monad" should {
    import Monad._

    "11.1a Option" in {
      optionMonad.flatMap(Option(10))(i => Option((i + 1).toString)) mustBe Option("11")
    }

    "11.1b List" in {
      listMonad.flatMap(List(1, 2, 3))(i => List((i + 1).toString)) mustBe List("2", "3", "4")
    }

    "11.1c LazyList" in {
      lazyListMonad.flatMap(LazyList(1, 2, 3))(i => LazyList((i + 1).toString)).toList mustBe List("2", "3", "4")
    }

    "11.2 state" in {
      val stateMonads = new StateMonads[Int]

      val s: stateMonads.StateS[String] = stateMonads.stateMonad.flatMap(State.unit("hi"))(a => State(s2 => (a + " bye", s2 + 5)))

      val (string, int) = s.run(6)

      string mustBe "hi bye"
      int mustBe 11
    }

    "11.3a sequence" in {
      val os: Option[List[Int]] = optionMonad.sequence(List(Some(1), Some(2), Some(3)))

      os mustBe Option(List(1, 2, 3))
    }

    "11.3b traverse" in {
      val os: Option[List[Int]] = optionMonad.traverse(List(1, 2, 3))(a => Option(a + 10))

      os mustBe Option(List(11, 12, 13))
    }

    "11.4 replicateM" in {
      val os: Option[List[String]] = optionMonad.replicateM(3, Option("hi"))

      os mustBe Option(List("hi", "hi", "hi"))
    }

    "11.5 replicateM for different Monads" in {
      /*
      For `List`, the `replicateM` function will generate a list of lists.
      It will contain all the lists of length `n` with elements selected from the input list.

      For `Option`, it will generate either `Some` or `None` based on whether the input is `Some` or `None`.
      The `Some` case will contain a list of length `n` that repeats the element in the input `Option`.

      The general meaning of `replicateM` is described very well by the implementation `sequence(List.fill(n)(ma))`.
      It repeats the `ma` monadic value `n` times and gathers the results in a single value, where the monad `M` determines how values are actually combined.
      */
    }

    "11.6 filterM" in {
      val ops: Option[List[Int]] = optionMonad.filterM(List(1, 2, 3))(a => if (a > 1) Option(true) else Option(false))

      ops mustBe Option(List(2, 3))
    }

    "11.7 kleisli" in {
      /*
      The Monad associate law states:
      x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))

      There’s a way we can make the law clearer if we consider not the monadic values of types like F[A], but monadic functions of types like A => F[B].
      Functions like that are called Kleisli arrows, and they can be composed with one another as in:

      def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C]

      We can now state the associative law for monads in a much more symmetric way:

      compose(compose(f, g), h) == compose(f, compose(g, h))

      which now has similarities to the easier to read Monoid associative law:

      op(op(x, y), z) == op(x, op(y, z))
      */
    }

    "11.8" in {

    }

    "11.9" in {

    }

    "11.10" in {

    }

    "11.11" in {

    }

    "11.12" in {

    }

    "11.13" in {

    }

    "11.14" in {

    }

    "11.15" in {

    }

    "11.16" in {

    }
  }
}

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: F[A] Either F[B]): F[A Either B] = e match {
    case Left(fa) => map(fa)(Left.apply)
    case Right(fb) => map(fb)(Right.apply)
  }
}

object Functor {
  def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]

  val listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}

object FunctorLaws {
  def map[F[_]: Functor, A](F: F[A]): Boolean =
    Functor[F].map(F)(identity) == F
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    @tailrec
    def go(lma: List[F[A]], acc: F[List[A]]): F[List[A]] = lma match {
      case Nil => acc
      case fa +: fas => go(fas, map2(acc, fa)(_ :+ _))
    }

    go(lma, unit(List.empty[A]))
  }

  def sequenceAlt[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List.empty[A])) {
      (ma, mla) => map2(ma, mla)(_ +: _)
    }

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List.empty[B])) {
      (a, acc) => map2(f(a), acc)(_ +: _)
    }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    @tailrec
    def go(n: Int, acc: F[List[A]]): F[List[A]] = n match {
      case i if i <= 0 => acc
      case i => go(i - 1, map2(ma, acc)((a, as) => as :+ a))
    }

    go(n, unit(List.empty[A]))
  }

  def replicateMAlt[A](n: Int, ma: F[A]): F[List[A]] =
    if (n <= 0) unit(List[A]()) else map2(ma, replicateMAlt(n - 1, ma))(_ +: _)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    ms.foldRight(unit(List.empty[A])) { (a, acc) =>
      compose(f, (b: Boolean) => if (b) map2(unit(a), acc)(_ +: _) else acc)(a)
    }
  }
}

object Monad {
  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Option(a)

    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma match {
      case None => None
      case Some(a) => f(a)
    }
  }

  val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)

    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma match {
      case Nil => Nil
      case a +: as => f(a) ++ flatMap(as)(f)
    }
  }

  val lazyListMonad: Monad[LazyList] = new Monad[LazyList] {
    def unit[A](a: => A): LazyList[A] = LazyList(a)

    def flatMap[A, B](ma: LazyList[A])(f: A => LazyList[B]): LazyList[B] = ma match {
      case as if as.isEmpty => LazyList.empty[B]
      case a #:: as => f(a) ++ flatMap(as)(f)
    }
  }

  /**
   * State is also a monad too, but it takes two type arguments and you need a type constructor of one argument to implement Monad, hence the type alias.
   *
   * So, since `State` is a binary type constructor, we need to partially apply it with the `S` type argument.
   * Thus, it is not just one monad, but an entire family of monads, one for each type `S`.
   * One solution is to create a class `StateMonads` that accepts the `S` type argument and then has a _type member_ for the fully applied `State[S, A]` type inside.
   */
  class StateMonads[S] {
    type StateS[A] = State[S, A]

    // We can then declare the monad for the `StateS` type constructor:
    val stateMonad: Monad[StateS] = new Monad[StateS] {
      def unit[A](a: => A): StateS[A] = State(s => (a, s))

      def flatMap[A, B](ma: StateS[A])(f: A => StateS[B]): StateS[B] =
        ma.flatMap(f)
    }
  }
}