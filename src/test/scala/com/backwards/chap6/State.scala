package com.backwards.chap6

case class State[S, +A](run: S => (A, S)) {
  import com.backwards.chap6.State._

  def mapFirstAttempt[B](f: A => B): State[S, B] = State { s =>
    val (a, newS) = run(s)
    (f(a), newS)
  }

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, newS) = run(s)
    f(a).run(newS)
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap { a =>
      sb.map { b =>
        f(a, b)
      }
    }
}

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](Nil)) { (f, acc) =>
      f.map2(acc)(_ +: _)
    }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
  _ <- set(f(s))
  } yield ()
}