package com.backwards.chap12

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.chap11.Functor

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