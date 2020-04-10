package com.backwards.chap7

import java.util.concurrent._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.chap7.Par._

class Chap7Spec extends AnyWordSpec with Matchers {
  println(s"Chap7Spec on thread ${Thread.currentThread.getName}")

  val executorService: ExecutorService = Executors.newFixedThreadPool(3)

  "Parallelism" should {
    "7.1 map2" in {
      def sum(ints: IndexedSeq[Int]): Par[Int] =
        if (ints.size <= 1) {
          unit(ints.headOption getOrElse 0)
        } else {
          val (l, r) = ints.splitAt(ints.length/2)
          map2(sum(l), sum(r))(_ + _)
        }

      val par: Par[Int] = sum(Vector(1, 2, 3, 4, 5))

      val future: Future[Int] = Par.run(executorService)(par)

      future.get mustBe 15
    }

    "7.2" in {
      // Think about design
    }

    "7.3 a new Future implementation where map2 respects timeouts" in {
      def sum(ints: IndexedSeq[Int]): Par[Int] =
        if (ints.size <= 1) {
          unit(ints.headOption getOrElse 0)
        } else {
          val (l, r) = ints.splitAt(ints.length/2)
          map2b(sum(l), sum(r))(_ + _)
        }

      val par: Par[Int] = sum(Vector(1, 2, 3, 4, 5))

      val future: Future[Int] = Par.run(executorService)(par)

      future.get(3, TimeUnit.SECONDS) mustBe 15
    }

    "7.4 asyncF" in {
      val fn: Int => String =
        _.toString

      val fnWithAsyncResult: Int => Par[String] =
        asyncF(fn)

      val par: Par[String] = fnWithAsyncResult(42)

      val future = Par.run(executorService)(par)

      future.get mustBe "42"
    }

    "7.5 sequence" in {
      // TODO
    }

    "7.6" in {
      // TODO
    }
  }
}

/**
 * "run" needs to execute asynchronous tasks somehow.
 * Let's use the Java Standard Library, java.util.concurrent.ExecutorService
 *
 * The simplest possible model for Par[A] might be ExecutorService => A, i.e.
 * def run[A](s: ExecutorService)(a: Par[A]): A
 *
 * This would obviously make run trivial to implement.
 *
 * But it might be nice to defer the decision of how long to wait for a computation,
 * or whether to cancel it, to the caller of run.
 * So Par[A] becomes ExecutorService => Future[A], and run simply returns the Future (as shown below).
 *
 * Par is represented by a function that needs an ExecutorService,
 * the creation of the Future doesn't actually happen until this ExectorService is provided.
 */
object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  /**
   * "unit" is represented as a function that returns a UnitFuture,
   * which is a simple implementation of Future that just wraps a constant value.
   * It doesn't use the ExecutorService at all.
   * It's always done and can't be cancelled.
   * Its "get" method simply returns the value that we gave it.
   */
  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  /**
   * Wraps the expression "a" for concurrent evaluation by "run"
   */
  def lazyUnit[A](a: A): Par[A] = fork(unit(a))

  /**
   * "map2" doesn't evaluate the call to "f" in a separate logical thread,
   * in accord with our design choice of having fork be the sole function in the API for controlling parallelism.
   * We can always do fork(map2(a, b)(f)) if we want the evaluation of f to occur in a separate thread.
   *
   * NOTE
   * This implementation of "map2" does not respect timeouts.
   * It simply passes the ExecutorService on to both Par values;
   * waits for the results of the Futures `af` and `bf`, applies `f` to them;
   * and wraps them in a `UnitFuture`.
   *
   * In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`,
   * then subtracts that time from the available time allocated for evaluating `bf`.
   */
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)

      UnitFuture(f(af.get, bf.get))
    }

  def map2b[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)

      new Future[C] {
        println(s"Future(${get()}) on thread ${Thread.currentThread.getName}")

        def cancel(mayInterruptIfRunning: Boolean): Boolean = true

        def isCancelled: Boolean = af.isCancelled || bf.isCancelled

        def isDone: Boolean = af.isDone && bf.isDone

        def get(): C = f(af.get, bf.get)

        def get(timeout: Long, unit: TimeUnit): C = {
          val started: Long = System.currentTimeMillis

          val a: A = af.get(timeout, unit)
          val elapsed: Long = System.currentTimeMillis - started
          val remaining: Long = unit.toMillis(timeout) - elapsed
          val b: B = bf.get(remaining, unit)

          f(a, b)
        }
      }
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call: A = a(es).get
    })

  /**
   * A function to convert any function A => B to one that evaluates its result asynchronously.
   */
  def asyncF[A, B](f: A => B): A => Par[B] =
    (a: A) => lazyUnit(f(a))

  /**
   * To implements a "sort", we could the Par, sort the resulting list, and repackage it in a Par with unit.
   * But we want to avoid calling run.
   * The only other combinator we have that allows us to manipulate the value of a Par in any way is map2.
   * So if we passed parList to one side of map2, we'd be able to gain access to the List inside and sort it.
   * And we can pass whatever we want to the other side of map2, so let’s just pass a no-op
   */
  def sortParUsingNoOp(parList: Par[List[Int]]): Par[List[Int]] =
    map2(parList, unit(()))((a, _) => a.sorted)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  // We can now implement sortPar with "map":
  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  // Map over a list in parallel
  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] =
    fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ???

  private case class UnitFuture[A](get: A) extends Future[A] {
    println(s"UnitFuture($get) on thread ${Thread.currentThread.getName}")

    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }
}