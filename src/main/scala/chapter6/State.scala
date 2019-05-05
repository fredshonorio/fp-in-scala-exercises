package chapter6

case class State[S, +A](run: S => (A, S)) {

  // ex 6.10
  def map[B](f: A => B): State[S, B] = State { s0 =>
    val (a, s1) = run(s0)
    (f(a), s1)
  }

  // ex 6.10
  def map2[B, C](sB:State[S, B])(f: (A, B) => C): State[S, C] = State { s0 =>
    val (a, s1) = run(s0)
    val (b, s2) = sB.run(s1)
    (f(a, b), s2)
  }

  // ex 6.10
  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s0 =>
    val (a, s1) = run(s0)
    f(a).run(s1)
  }

}

object State {

  // ex 6.10
  def sequence[S, A](xs: List[State[S, A]]): State[S, List[A]] =
    xs.foldRight(unit[S, List[A]](List())) { (r, rl) => r.map2(rl)(_ :: _) }

  // ex 6.10
  def unit[S, A](v: A): State[S, A] = State(s => (v, s))


  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}
