package com.backwards.chap7

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Chap7Spec extends AnyWordSpec with Matchers {
  println(s"Chap7Spec on thread ${Thread.currentThread.getName}")

  val executorService: ExecutorService = Executors.newFixedThreadPool(3)

  "Parallelism" should {
    import com.backwards.chap7.Par._

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
      val par: Par[List[Int]] = sequence(List(unit(1), unit(2), unit(3)))

      val future: Future[List[Int]] = Par.run(executorService)(par)

      future.get() mustBe List(1, 2, 3)
    }

    "7.6 parFilter" in {
      val par: Par[List[Int]] = parFilter(List(1, 2, 3, 4, 5))(_ % 2 == 0)

      val future: Future[List[Int]] = Par.run(executorService)(par)

      future.get() mustBe List(2, 4)
    }

    "7.7" in {
      // Think about design
    }

    "7.8 executors and an initial bug in fork - the following will deadlock" in {
      val a = lazyUnit(42 + 1)

      val S = Executors.newFixedThreadPool(1)

      // The following deadlocks as 2 threads would need to be available but there is only 1 in the pool
      // Par.equal(S)(a, fork(a)) mustBe true
    }

    "7.9" in {
      // 7.8 highlights the fact that using a fixed size thread pool can cause our original implementaiton of "fork" to deadlock
      // Next exercise (in a separate "should") we explore implementation via "actors".
    }

    "7.11 choiceN" in {
      val par: Par[String] = choiceN(lazyUnit(3))(List(lazyUnit("a"), lazyUnit("b"), lazyUnit("c"), lazyUnit("d"), lazyUnit("e")))

      val future: Future[String] = Par.run(executorService)(par)

      future.get() mustBe "d"
    }

    "7.11b choice using choiceN" in {
      val par: Par[String] = choiceUsingChoiceN(lazyUnit(false))(lazyUnit("c"), lazyUnit("d"))

      val future: Future[String] = Par.run(executorService)(par)

      future.get() mustBe "d"
    }

    "7.12 choiceMap" in {
      val par: Par[String] = choiceMap[Int, String](lazyUnit(3))(Map(0 -> lazyUnit("a"), 1 -> lazyUnit("b"), 2 -> lazyUnit("c"), 3 -> lazyUnit("d"), 4 ->lazyUnit("e")))

      val future: Future[String] = Par.run(executorService)(par)

      future.get() mustBe "d"
    }

    "7.13 chooser" in {
      val par: Par[String] = chooser(lazyUnit(3))(Map(0 -> lazyUnit("a"), 1 -> lazyUnit("b"), 2 -> lazyUnit("c"), 3 -> lazyUnit("d"), 4 ->lazyUnit("e")).apply)

      val future: Future[String] = Par.run(executorService)(par)

      future.get() mustBe "d"
    }

    "7.14 flatMap" in {
      val par: Par[String] = flatMap(lazyUnit(3))(Map(0 -> lazyUnit("a"), 1 -> lazyUnit("b"), 2 -> lazyUnit("c"), 3 -> lazyUnit("d"), 4 ->lazyUnit("e")).apply)

      val future: Future[String] = Par.run(executorService)(par)

      future.get() mustBe "d"
    }

    "7.14b join" in {
      val par: Par[Int] = join(lazyUnit(lazyUnit(3)))

      val future: Future[Int] = Par.run(executorService)(par)

      future.get() mustBe 3
    }
  }

  // The essential problem with the current representation is that we can't get a value out of a Future without the current thread blocking on its get method.
  // A representation of Par that doesn't leak resources this way has to be non-blocking,
  // in the sense that the implementations of fork and map2 must never call a method that blocks the current thread like Future.get.
  "Parallelism with actors" should {
    import com.backwards.chap7.ParActor._

    "example" in {
      val S = Executors.newFixedThreadPool(4)

      val echoer = Actor[String](S) {
        msg => println (s"Got message: '$msg'")
      }

      echoer ! "hello"
      // PRINT OUT: Got message: 'hello'

      echoer ! "goodbye"
      // PRINT OUT: Got message: 'goodbye'
    }

    "example showing multiple parallel computations using only 2 threads" in {
      val p = parMap(1 to 1000 toList)(math.sqrt(_))
      // p: ExecutorService => Future[List[Double]] = < function >

      // This will call fork about 1000 times, starting that many actors to combine these values two at a time.
      // Thanks to our non-blocking Actor implementation, we don't need 1000 JVM threads to do that in.
      val x = ParActor.run(Executors.newFixedThreadPool(2))(p)
      // PRINT OUT x: List[Double] = List(1.0, 1.4142135623730951, 1.7320508075688772, 2.0, 2.23606797749979, 2.449489742783178, 2.6457513110645907, 2.828 4271247461903, 3.0, 3.1622776601683795, 3.3166247903554, 3.46410...
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

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List.empty[A])) { (p, acc) =>
      map2(acc, p) { (as, a) => a +: as}
    }

  // This implementation forks the recursive step off to a new logical thread, making it effectively tail-recursive.
  // However, we are constructing a right-nested parallel program, and we can get better performance by dividing the list in half, and running both halves in parallel.
  // See `sequenceBalanced` below.
  def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
    as match {
      case Nil => unit(Nil)
      case h +: t => map2(h, fork(sequenceRight(t)))(_ +: _)
    }

  // We define `sequenceBalanced` using `IndexedSeq`, which provides an efficient function for splitting the sequence in half.
  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) {
      unit(Vector.empty)
    } else if (as.length == 1) {
      map(as.head)(a => Vector(a))
    } else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequenceAlt[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    map(
      sequence(
        as map asyncF(a => List(a) filter f)
      )
    )(_.flatten)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  // This constructs a computation that proceeds with "t" if cond results in true, or "f" if cond results in false.
  // We can implement this by blocking on the result of the cond and then using the result to determine whether to run "t" or "f".
  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => if (run(es)(cond).get) t(es) else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => run(es)(choices(n(es).get()))

  def choiceUsingChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(bool => if (bool) 0 else 1))(List(t, f))

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => run(es)(choices(key(es).get()))

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => run(es)(choices(pa(es).get()))

  def choiceUsingChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(a => if (a) t else f)

  def choiceNUsingChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(choices)

  /**
   * chooser is actually the more usual flatMap
   *
   * i.e. chooser is no longer the most appropriate name for this operation, which is actually quite general.
   * It's a parallel computation that, when run, will run an initial computation whose result is used to determine a second computation.
   * Nothing says that this second computation needs to even exist before the first computation’s result is available.
   * It doesn’t need to be stored in a container like List or Map.
   */
  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    es => run(es)(f(a(es).get()))

  /**
   * The name flatMap is suggestive of the fact that this operation could be decomposed into two steps:
   * mapping f: A => Par[B] over our Par[A], which generates a Par[Par[B]],
   * and then flattening this nested Par[Par[B]] to a Par[B].
   * We call it join since conceptually it’s a parallel computation that, when run, will execute the inner computation,
   * wait for it to finish (much like Thread.join), and then return its result.
   */
  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(a(es).get())

  def joinUsinFlatMap [A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(identity)

  private case class UnitFuture[A](get: A) extends Future[A] {
    println(s"UnitFuture($get) on thread ${Thread.currentThread.getName}")

    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }
}

/**
 * How can we implement a non-blocking representation of Par? The idea is simple.
 * Instead of turning a Par into a java.util.concurrent.Future that we can get a value out of (which requires blocking),
 * we'll introduce our own version of Future with which we can register a callback that will be invoked when the result is ready.
 *
 * The Future type we defined here is rather imperative. An A => Unit?
 * Such a function can only be useful for executing some side effect using the given A, as we certainly aren't using the returned result.
 * Are we still doing functional programming in using a type like Future?
 * Yes, but we're making use of a common technique of using side effects as an implementation detail for a purely functional API.
 */
object ParActor {
  sealed trait Future[A] {
    /**
     * The apply method is declared private to the chap7 package,
     * which means that it can only be accessed by code within that package.
     */
    private[chap7] def apply(k: A => Unit): Unit
  }

  /**
   * Par looks the same, but we're using our new non-blocking Future instead of the one in java.util.concurrent.
   */
  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    // Mutable thread-safe reference for storing the result.
    val ref: AtomicReference[A] = new AtomicReference[A]

    // A java.util.concurrent.CountDownLatch allows threads to wait until its countDown method is called a certain number of times.
    // Here the countDown method will be called once when we've received the value of type A from p,
    // and we want the run implementation to block until that happens.
    val latch: CountDownLatch = new CountDownLatch(1)

    // When we receive the value, sets the result and releases the latch.
    p(es) { a =>
      ref.set(a)
      latch.countDown()
    }

    // Waits until the result becomes available and the latch is released.
    latch.await()

    // Once we've passed the latch, we know ref has been set, and we return its value.
    ref.get
  }

  // Simply passes the value to the continuation. Note that the ExecutorService isn't needed.
  def unit[A](a: A): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit = cb(a)
    }

  def lazyUnit[A](a: A): Par[A] = fork(unit(a))

  // eval forks off evaluation of a and returns immediately. The callback will be invoked asynchronously on another thread.
  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        eval(es)(a(es)(cb))
    }

  // A helper function to evaluate an action asynchronously using some ExecutorService.
  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {
      def call: Unit = r
    })

  def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
    es => new Future[C] {
      def apply(cb: C => Unit): Unit = {
        // Two mutable vars are used to store the two results.
        var ar: Option[A] = None
        var br: Option[B] = None

        // An actor that awaits both results, combines them with f, and passes the result to cb.
        val combiner = Actor[A Either B](es) {
          // If the A result came in first, stores it in ar and waits for the B.
          // If the A result came last and we already have our B, calls f with both results and passes the resulting C to the callback, cb.
          case Left(a) => br match {
            case None => ar = Some(a)
            case Some(b) => eval(es)(cb(f(a, b)))
          }

          // Analogously, if the B result came in first, stores it in br and waits for the A.
          // If the B result came last and we already have our A, calls f with both results and passes the resulting C to the callback, cb.
          case Right(b) => ar match {
            case None => br = Some(b)
            case Some(a) => eval(es)(cb(f(a, b)))
          }
        }

        // Passes the actor as a continuation to both sides.
        // On the A side, we wrap the result in Left, and on the B side, we wrap it in Right.
        // These are the constructors of the Either data type, and they serve to indicate to the actor where the result came from.
        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }

  /**
   * A function to convert any function A => B to one that evaluates its result asynchronously.
   */
  def asyncF[A, B](f: A => B): A => Par[B] =
    (a: A) => lazyUnit(f(a))

  // Map over a list in parallel
  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] =
    fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List.empty[A])) { (p, acc) =>
      map2(acc, p) { (as, a) => a +: as}
    }
}