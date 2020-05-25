package com.backwards.chap12

import java.util.Date
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.chap11.Functor
import java.text._
import scala.util.Try
import com.backwards.chap10.{Foldable, Monoid}

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
    import Applicative._
    import Traverse._

    // TODO - All the following needs underlying to be reimplemented to avoid stack overflow from recursive calls
    "12.1a sequence" in {
      val v: Option[List[Int]] = optionApplicative.sequence(List(Some(1), Some(2), Some(3)))

      v mustBe Some(List(1, 2, 3))
    }

    "12.1b replicateM" in {
      val v: Option[List[String]] = optionApplicative.replicateM(3, Some("hi"))

      v mustBe Some(List("hi", "hi", "hi"))
    }

    "12.1c product" in {
      val v: Option[(Int, Int)] = optionApplicative.product(Some(1), Some(2))

      v mustBe Some(1 -> 2)
    }

    "12.2 apply" in {
      val v: Option[String] = optionApplicative.apply(Option((i: Int) => i.toString))(Option(42))

      v mustBe Some("42")
    }

    "12.3a map3" in {
      val v: Option[Int] = optionApplicative.map3(Option(1), Option(2), Option(3))(_ + _ + _)

      v mustBe Some(6)
    }

    "12.3b map4" in {
      val v: Option[Int] = optionApplicative.map4(Option(1), Option(2), None: Option[Int], Option(3))(_ + _ + _ + _)

      v mustBe None
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

    "12.7" in {

    }

    "12.8 product of applicatives" in {
      val applicative: Applicative[Lambda[x => (Option[x], String Either x)]] =
        optionApplicative product eitherApplicatve[String]

      applicative.unit(5) mustBe (Some(5), Right(5))
    }

    "12.9 compose" in {
      val applicative: Applicative[Lambda[x => Option[String Either x]]] =
        optionApplicative compose eitherApplicatve[String]

      applicative.unit(5) mustBe Some(Right(5))
    }

    "12.10" in {

    }

    "12.11" in {

    }

    /**
     * {{{
     *  Map[K, Par[A]] => Par[Map[K,A]]
     * }}}
     * A call to {{{Traverse[Map[K, _]].sequence}}} with Par as the Applicative
     * produces a parallel computation that evaluates all values of the map in parallel.
     */
    "12.12 sequenceMap" in {
      val option: Option[Map[Int, String]] =
        optionApplicative sequenceMap Map(42 -> Some("hi"), 52 -> Some("no"))

      option mustBe Some(Map(42 -> "hi", 52 -> "no"))
    }

    /**
     * {{{
     *  List[Option[A]] => Option[List[A]]
     * }}}
     * A call to Traverse[List].sequence with Option as the Applicative
     * returns None if any of the input List is None;
     * otherwise it returns the original List wrapped in Some.
     */
    "12.13a traverse instance for list" in {
      implicit val applicative: Applicative[Option] = optionApplicative

      val v: Option[List[Int]] = listTraverse.traverse(List(1, 2, 3))(a => Option(a))

      v mustBe Some(List(1, 2, 3))
    }

    "12.13b traverse instance for option" in {
      implicit val applicative: Applicative[Option] = optionApplicative

      val v: Option[Option[Int]] = optionTraverse.traverse(Option(1))(a => Option(a))

      v mustBe Some(Some(1))
    }

    /**
     * {{{
     *  Tree[Option[A]] => Option[Tree[A]]
     * }}}
     * Acall to Traverse[Tree].sequence with Option as the Applicative)
     * returns None if any of the input Tree is None;
     * otherwise it returns the original Tree wrapped in Some.
     */
    "12.13c traverse instance for tree" in {
      implicit val applicative: Applicative[Option] = optionApplicative

      val v: Option[Tree[Int]] = treeTraverse.traverse(Tree(1, List(Tree(2, Nil), Tree(3, Nil))))(a => Option(a))

      v mustBe Some(Tree(1, List(Tree(2, Nil), Tree(3, Nil))))
    }

    "12.14 traverse function is a generalisation of map" in {
      val v: Option[String] = optionTraverse.map(Option(42))(i => i.toString)

      v mustBe Option("42")
    }

    "12.15" in {
      // TODO - This is where I got lost
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
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] /*=
    apply(map(fa)(f.curried))(fb)*/

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

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
        a +: listOfA
      }
    }

  def sequenceBetter[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldRight(unit(Map.empty[K, V])) {
      case ((k, fv), fbs) =>
        map2(fv, fbs) { case (v, fb) =>
          fb + (k -> v)
        }
    }

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

      def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = ???
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

  // Here we simply use `map2` to lift `apply` and `unit` themselves from one Applicative into the other.
  // If `self` and `G` both satisfy the laws, then so does the composite.
  // The full proof can be found at
  // https://github.com/runarorama/sannanir/blob/master/Applicative.v

  // def compose[G[_]](G: Applicative[G]): Applicative[({ type f[x] = F[G[x]] })#f]
  def compose[G[_]](G: Applicative[G]): Applicative[Lambda[x => F[G[x]]]] = {
    val self = this

    // new Applicative[({ type f[x] = F[G[x]] })#f]
    new Applicative[Lambda[x => F[G[x]]]] {
      def unit[A](a: => A): F[G[A]] =
        self.unit(G.unit(a))

      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fga, fgb)(G.map2(_, _)(f))
    }
  }
}

object Applicative {
  val optionApplicative: Applicative[Option] = new Applicative[Option] {
    def unit[A](a: => A): Option[A] = Option(a)

    def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] = (fa, fb) match {
      case (Some(a), Some(b)) => Option(f(a, b))
      case _ => None
    }
  }

  def eitherApplicatve[E]: Applicative[E Either *] = new Applicative[E Either *] {
    def unit[A](a: => A): E Either A = Right(a)

    def map2[A, B, C](fa: E Either A, fb: E Either B)(f: (A, B) => C): E Either C = (fa, fb) match {
      case (Right(a), Right(b)) => Right(f(a, b))
      case (Left(e), _) => Left(e)
      case (_, Left(e)) => Left(e)
    }
  }
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

case class Failure[E](head: E, tail: Vector[E] = Vector.empty[E]) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(identity)

  def map[A, B](fa: F[A])(f: A => B): F[B] = ???

  // traverse is more general than map.
  // traverse can also express foldMap and by extension foldLeft and foldRight!

  // Suppose that our G were a type constructor ConstInt that takes any type to Int, so that
  // ConstInt[A] throws away its type argument A and just gives us Int:
  // type ConstInt[A] = Int
  // Then in the type signature for traverse, if we instantiate G to be ConstInt, it becomes
  // def traverse[A, B](fa: F[A])(f: A => Int): Int
  // This looks a lot like foldMap from Foldable.
  // Indeed, if F is something like List, then what we need to implement this signature is a way of combining the Int values returned by f for each element of the list,
  // and a “starting” value for handling the empty list. In other words, we only need a Monoid[Int]

  // Turning a Monoid into an Applicative:
  type Const[M, B] = M

  // implicit def monoidApplicative[M](M: Monoid[M]): Applicative[({ type f[x] = Const[M, x] })#f] = new Applicative[({ type f[x] = Const[M, x] })#f] {
  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[Const[M, *]] =
    new Applicative[Const[M, *]] {
      def unit[A](a: => A): M = M.zero

      def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M = M.op(m1,m2)
    }

  // This means that Traverse can extend Foldable and we can give a default implemen- tation of foldMap in terms of traverse:
  // override def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M = traverse[({ type f[x] = Const[M,x] })#f, A, Nothing](as)(f)(monoidApplicative(mb))
  override def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[Const[M, *], A, Nothing](as)(f)(monoidApplicative(mb))
}

object Traverse {
  val listTraverse: Traverse[List] = new Traverse[List] {
    override def map[A, B](as: List[A])(f: A => B): List[B] = as map f

    override def traverse[G[_]: Applicative, A, B](as: List[A])(f: A => G[B]): G[List[B]] = {
      val applicative = implicitly[Applicative[G]]

      as.foldRight(applicative.unit(List.empty[B])) { case (a, g) =>
        applicative.map2(f(a), g) { case (b, bs) =>
          b +: bs
        }
      }
    }
  }

  val optionTraverse: Traverse[Option] = new Traverse[Option] {
    // Original and simplest implementation
    // override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f

    // Implemented in terms of traverse for exercise 12.14
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = {
      implicit val optionApplicative: Applicative[Option] = Applicative.optionApplicative

      traverse(fa)(a => optionApplicative.unit(f(a))) match {
        case Some(r) => r
        case _ => None
      }
    }

    override def traverse[G[_]: Applicative, A, B](oa: Option[A])(f: A => G[B]): G[Option[B]] = {
      val applicative = implicitly[Applicative[G]]

      oa match {
        case Some(a) => applicative.map(f(a))(Some.apply)
        case None => applicative.unit(None)
      }
    }
  }

  val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
    override def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
      Tree(f(tree.head), tree.tail.map(map(_)(f)))

    override def traverse[G[_]: Applicative, A, B](tree: Tree[A])(f: A => G[B]): G[Tree[B]] = {
      val applicative: Applicative[G] = implicitly[Applicative[G]]

      applicative.map2(f(tree.head), listTraverse.traverse(tree.tail)(a => traverse(a)(f)))(Tree.apply)
    }
  }
}