package com.backwards.chap12

import java.util.Date
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.chap11.Functor
import java.text._
import scala.util.Try

/**
 * A large number of the useful combinators on Monad can be defined using only unit and map2.
 * The traverse combinator is one example — it doesn’t call flatMap directly and is therefore agnostic to whether map2 is primitive or derived.
 * Furthermore, for many data types, map2 can be implemented directly, without using flatMap.
 *
 * All this suggests a variation on Monad — the Monad interface has flatMap and unit as primitives,
 * and derives map2, but we can obtain a different abstraction by letting unit and map2 be the primitives.
 *
 * Here we’ll see that this new abstraction, called an applicative functor, is less powerful than a monad, but that limitations come with benefits.
 */
class Chap12Spec extends AnyWordSpec with Matchers {
  "Applicative Functor" should {
    "12.1a sequence" in {

    }

    "12.1b replicateM" in {

    }

    "12.1c product" in {

    }

    "12.2 apply" in {

    }

    "12.3a map3" in {

    }

    "12.3b map4" in {

    }

    "12.5 monad instance of Either" in {
      // def eitherMonad[E]: Monad[({ type M[x] = Either[E, x] })#M] = ???
      def eitherMonad[E]: Monad[E Either *] = new Monad[E Either *] {
        def unit[A](a: => A): E Either A = Right(a)

        override def flatMap[A, B](ma: E Either A)(f: A => E Either B): E Either B = ma match {
          case Right(a) => f(a)
          case Left(e) => Left(e)
        }
      }

      eitherMonad.unit(42) mustBe Right(42)
    }

    // def validationApplicative[E]: Applicative[({ type A[x] = Validation[E, x] })#A] = ???
    def validationApplicative[E]: Applicative[E Validation *] = new Applicative[E Validation *] {
      def unit[A](a: => A): E Validation A = Success(a)

      override def map2[A, B, C](fa: E Validation A, fb: E Validation B)(f: (A, B) => C): E Validation C = (fa, fb) match {
        case (Success(a), Success(b)) => Success(f(a, b))
        case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
        case (failure @ Failure(_, _), _) => failure
        case (_, failure @ Failure(_, _)) => failure
      }
    }

    "12.6 applicative instance of Validation" in {
      val v: Validation[String, Int] = validationApplicative[String].map2[Int, Int, Int](Failure("whoops1"), Failure("whoops2"))(_ + _)

      v mustBe Failure("whoops1", Vector("whoops2"))
    }

    "Web form example" should {
      case class WebForm(name: String, birthdate: Date, phoneNumber: String)

      // This data will likely be collected from the user as strings, and we must make sure that the data meets a certain specification.
      // If it doesn’t, we must give a list of errors to the user indicating how to fix the problem.
      // The specification might say that name can’t be empty, that birthdate must be in the form "yyyy-MM-dd", and that phoneNumber must contain exactly 10 digits.

      def validName(name: String): String Validation String =
        if (name != "") Success(name)
        else Failure("Name cannot be empty")

      def validBirthdate(birthdate: String): String Validation Date =
        Try {
          Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthdate))
        } getOrElse Failure("Birthdate must be in the form yyyy-MM-dd")

      def validPhone(phoneNumber: String): String Validation String =
        if (phoneNumber.matches("[0-9]{10}")) Success(phoneNumber)
        else Failure("Phone number must be 10 digits")

      // And to validate an entire web form, we can simply lift the WebForm constructor with map3:

      def validWebForm(name: String, birthdate: String, phone: String): String Validation WebForm =
        validationApplicative.map3(
          validName(name),
          validBirthdate(birthdate),
          validPhone(phone)
        )(WebForm)
    }
  }

  "Streams are applicative functors but not monads" should {
    val lazyListApplicative = new Applicative[LazyList] {
      def unit[A](a: => A): LazyList[A] =
        LazyList.continually(a)

      override def map2[A, B, C](fa: LazyList[A], fb: LazyList[B])(f: (A, B) => C): LazyList[C] =
        fa zip fb map f.tupled

      override def sequence[A](fas: List[LazyList[A]]): LazyList[List[A]] =
        fas.foldLeft(unit(List.empty[A])) { (acc, l) =>
          acc #::: l.map(List(_))
        }
    }

    "unit" in {
      lazyListApplicative.unit(42).take(3).toList mustBe List(42, 42, 42)
    }

    "map2" in {
      val xs: LazyList[Int] = lazyListApplicative.map2(LazyList(1, 2), LazyList(5, 6))(_ + _)

      xs.toList mustBe List(6, 8)
    }
  }
}

trait Applicative[F[_]] extends Functor[F] {
  /**
   * `map2` is implemented by first currying `f` so we get a function of type `A => B => C`.
   * This is a function that takes `A` and returns another function of type `B => C`.
   * So if we map `f.curried` over an `F[A]`, we get `F[B => C]`.
   * Passing that to `apply` along with the `F[B]` will give us the desired `F[C]`.
   */
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa) { (f, a) =>
      // where f: A => B and a: A
      f(a)
    }

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B]))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight(unit(List.empty[A])) { (fa, acc) =>
      map2(fa, acc) { (a, listOfA) =>
        listOfA :+ a
      }
    }

  def sequenceBetter[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb) { (a, b) =>
      a -> b
    }

  // Without Kind Projector:
  // def product[G[_]](G: Applicative[G]): Applicative[({ type f[x] = (F[x], G[x]) })#f]
  def product[G[_]](G: Applicative[G]): Applicative[Lambda[x => (F[x], G[x])]] = {
    val self = this

    // Without Kind Projector:
    // new Applicative[({ type f[x] = (F[x], G[x]) })#f]
    new Applicative[Lambda[x => (F[x], G[x])]] {
      def unit[A](a: => A): (F[A], G[A]) =
        (self.unit(a), G.unit(a))

      override def apply[A,B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
    }
  }

  def map3[A, B, C, D](
    fa: F[A],
    fb: F[B],
    fc: F[C]
  )(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit[A => B => C => D](f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](
    fa: F[A],
    fb: F[B],
    fc: F[C],
    fd: F[D]
  )(f: (A, B, C, D) => E): F[E] =
    // We don't actually have to annotate unit
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
}

// From previous chapter, we can now extend Monad from Applicative Functor:
trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(f))

  override def map[A, B](m: F[A])(f: A => B): F[B] =
    flatMap(m)(a => unit(f(a)))

  override def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(ma => ma)
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]