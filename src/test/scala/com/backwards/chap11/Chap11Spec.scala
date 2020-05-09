package com.backwards.chap11

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Chap11Spec extends AnyWordSpec with Matchers {
  //new Functor.Fun
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
