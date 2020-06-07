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

    "11.8 flatMap using compose and unit" in {
      val ops: Option[Int] = optionMonad.flatMapUsingCompose(Some(1))(i => Some(i + 1))

      ops mustBe Option(2)
    }

    "11.12 join" in {
      val ops: Option[Int] = optionMonad.join(Option(Option(42)))

      ops mustBe Option(42)
    }

    "11.13 flatMap using join and map" in {
      val ops: Option[Int] = optionMonad.flatMapUsingJoinAndMap(Some(1))(i => Some(i + 1))

      ops mustBe Option(2)
    }

    "11.17 Identity Monad" in {
      val identityMonad = new Monad[Id] {
        def unit[A](a: => A): Id[A] = Id(a)

        def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma flatMap f
      }

      val x: Id[Int] = identityMonad.map(Id(42))(_ + 1)

      x mustBe Id(43)

      val y: Id[Int] = identityMonad.flatMap(Id(42))(i => Id(i + 1))

      y mustBe Id(43)
    }
  }

  "State Monad" should {
    case class State[S, A](run: S => (A, S)) {
      def map[B](f: A => B): State[S, B] =
        State { s =>
          val (a, s1) = run(s)
          (f(a), s1)
        }

      def flatMap[B](f: A => State[S, B]): State[S, B] =
        State { s =>
          val (a, s1) = run(s)
          f(a).run(s1)
        }
    }

    /*
    It looks like State fits the profile for being a monad.
    But its type constructor takes two type arguments, and Monad requires a type constructor of one argument, so we can’t just say Monad[State].
    But if we choose some particular S, then we have something like State[S, _], which is the kind of thing expected by Monad.
    So State doesn’t just have one monad instance but a whole family of them, one for each choice of S.
    We’d like to be able to partially apply State to where the S type argument is fixed to be some concrete type.

    This is much like how we might partially apply a function, except at the type level.
    E.g. we can create an IntState type constructor, which is an alias for State with its first type argument fixed to be Int:
    */

    type IntState[A] = State[Int, A]

    // And IntState is exactly the kind of thing that we can build a Monad for:
    object IntStateMonad extends Monad[IntState] {
      def unit[A](a: => A): IntState[A] = State(s => (a, s))

      def flatMap[A,B](st: IntState[A])(f: A => IntState[B]): IntState[B] =
        st flatMap f
    }

    /*
    Of course, it would be really repetitive if we had to manually write a separate Monad instance for each specific state type.
    Unfortunately, Scala doesn’t allow us to use underscore syntax to simply say State[Int, _] to create an anonymous type constructor like we create anonymous functions.
    But instead we can use something similar to lambda syntax at the type level. For example, we could have declared IntState directly inline like this:
    */

    object IntStateMonadViaLambda extends Monad[({ type IntState[A] = State[Int, A] })#IntState] {
      def unit[A](a: => A): IntState[A] = State(s => (a, s))

      def flatMap[A,B](st: IntState[A])(f: A => IntState[B]): IntState[B] =
        st flatMap f
    }

    /*
    All we’re doing is declaring an anonymous type within parentheses.
    This anonymous type has, as one of its members, the type alias IntState, which looks just like before.
    Outside the parentheses we’re then accessing its IntState member with the # syntax.
    Just like we can use a dot (.) to access a member of an object at the value level, we can use the # symbol to access a type member.

    A type constructor declared inline like this is often called a type lambda in Scala.
    We can use this trick to partially apply the State type constructor and declare a StateMonad trait.
    An instance of StateMonad[S] is then a monad instance for the given state type S:
    */

    def stateMonad[S] = new Monad[({ type M[x] = State[S,x] })#M] {
      def unit[A](a: => A): State[S,A] = State(s => (a, s))

      def flatMap[A,B](st: State[S,A])(f: A => State[S,B]): State[S,B] = st flatMap f
    }

    // Note we can use Kind Projector to actually achieve what we want.

    object IntStateMonadViaKindProjectorVersionOfLambda extends Monad[State[Int, *]] {
      def unit[A](a: => A): IntState[A] = State(s => (a, s))

      def flatMap[A,B](st: IntState[A])(f: A => IntState[B]): IntState[B] =
        st flatMap f
    }
  }

  "Reader Monad" should {
    case class Reader[R, A](run: R => A)

    object Reader {
      def readerMonad[R]: Monad[Reader[R, *]] = new Monad[Reader[R, *]] {
        def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

        def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
          Reader(r => f(st.run(r)).run(r))
      }
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

  def flatMapUsingCompose[A, B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => ma, f)(())

  def flatMapUsingJoinAndMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

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

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(identity)
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

// Identity Monad:
final case class Id[A](value: A) {
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)

  def map[B](f: A => B): Id[B] = Id(f(value))
}