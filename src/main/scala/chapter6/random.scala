package chapter6

object random {

  trait RNG {
    def nextInt: (Int, RNG)

    // ex 6.1
    def nonNegativeInt: (Int, RNG) = {
      val (n, rng) = nextInt
      // abs(Int.MinValue) does not have fit in an Int
      // we split the set of numbers in two halves of the same size
      // [MinValue, -1] and [0, MaxValue], and map the first one to the second one
      (
        if (n < 0) math.abs(n + 1) else n,
        rng
      )
    }

    // ex 6.2
    def double: (Double, RNG) = {
      val (n, rng) = nonNegativeInt
      val d = n / (Int.MaxValue.toDouble + 1) // n can be MaxValue, so we divide by MaxValue + 1
      (d, rng)
    }

    // ex 6.3
    def intDouble: ((Int, Double), RNG) = {
      val (i, r0) = nextInt
      val (d, r1) = double
      ((i, d), r1)
    }

    // ex 6.3
    def doubleInt: ((Double, Int), RNG) = {
      val (t, r) = intDouble
      (t.swap, r)
    }

    // ex 6.3
    def double3: ((Double, Double, Double), RNG) = {
      val (d0, r0) = double
      val (d1, r1) = double
      val (d2, r2) = double
      ((d0, d1, d2), r2)
    }

    // ex 6.4
    def ints(n: Int): (List[Int], RNG) = {
      if (n <= 0) (List.empty, this)
      else
        List.range(0, n)
          .scanRight((List.empty[Int], this)) { case (_, (xs, rng)) => {
            val (i, r) = rng.nextInt
            (i :: xs, r)
          } }.head
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  object Rand {
    val int: Rand[Int] = _.nextInt
    val nonNegativeInt: Rand[Int] = map(int)(n => if (n < 0) math.abs(n + 1) else n)

    def unit[A](a: A): Rand[A] =
      rng => (a, rng)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      rng0 => {
        val (a, rng1) = s(rng0)
        (f(a), rng1)
      }

    // ex 6.5
    def double: Rand[Double] = map(nonNegativeInt)(n => n / (Int.MaxValue.toDouble + 1))

    // ex 6.6
    def map2[A, B, C](rA: Rand[A], rB: Rand[B])(f: (A, B) => C): Rand[C] = rng0 => {
      val (a, rng1) = rA(rng0)
      val (b, rng2) = rB(rng1)
      (f(a, b), rng2)
    }

    // ex 6.7
    def sequence[A](xs: List[Rand[A]]): Rand[List[A]] = {
      xs.foldRight(Rand.unit(List[A]())) { (r, rl) => map2(r, rl)((x, xs) => x :: xs) }
    }

    // ex 6.7
    def ints(n: Int): Rand[List[Int]] = sequence(List.fill(n)(int))

    // ex 6.8
    def flatMap[A, B](rA: Rand[A])(f: A => Rand[B]): Rand[B] = rng0 => {
      val (a, rng1) = rA(rng0)
      f(a)(rng1)
    }

    // ex 6.8
    def nonNegativeLessThan(n: Int): Rand[Int] = {
      flatMap(Rand.int) { i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)

      }
    }

    // ex 6.9
    def mapFm[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))
    def map2Fm[A, B, C](rA: Rand[A], rB: Rand[B])(f: (A, B) => C): Rand[C] = {
      flatMap(rA)(a => mapFm(rB)(b => f(a, b)))
    }

  }

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRng = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRng)
    }
  }
}
