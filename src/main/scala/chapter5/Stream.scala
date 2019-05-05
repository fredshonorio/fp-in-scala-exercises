package chapter5

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def toListR(): List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toListR()
  }

  def toList(): List[A] = {

    @tailrec
    def go(xs: Stream[A], acc: List[A]): List[A] = xs match {
      case Empty => acc
      case Cons(h, t) => go(t(), h() :: acc)
    }

    // reversing looks silly, we might as well to append directly,
    // but prepend on list is O(1) and append is O(n) so this way is considerably faster
    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n >= 1 => Stream.cons[A](h(), t().take(n - 1))
      case _ => Empty
    }
  }

  def drop(n: Int): Stream[A] = {
    @tailrec
    def go(n: Int, xs: Stream[A]): Stream[A] = {
      if (n == 0) xs
      else
        xs match {
          case Empty => xs
          case Cons(_, t) => go(n - 1, t())
        }
    }

    go(n, this)
  }

  def takeWhile(pred: A => Boolean): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h, t) => {
        val hh = h()
        if (pred(hh)) Stream.cons(hh, t().takeWhile(pred))
        else Empty
      }
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case Empty => z
    }

  def forAll(p: A => Boolean): Boolean =
    this match {
      case Empty => true
      case Cons(h, t) => p(h()) && t().forAll(p)
    }

  // in terms of foldRight
  def forAll_(p: A => Boolean) : Boolean =
    foldRight(true)((z, x) => p(z) && x)

  // in terms of foldRight
  def takeWhile_(p: A => Boolean) : Stream[A] =
    foldRight(Stream[A]())((x, z) =>
      if (p(x))
        Stream.cons(x, z)
      else
        Stream[A]()
    )

  def headOption() : Option[A] =
    foldRight(Option.empty[A])((x, _) => Option(x))

  // ex 5.7
  // implement in terms of foldRight
  def map[B](f: A => B): Stream[B] = foldRight(Stream[B]())((h, t) => Stream.cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream[A]())((h, t) => if (f(h)) Stream.cons(h, t) else t)

  def append[B >: A](xs: => Stream[B]): Stream[B] =
    foldRight(xs)((h, t) => Stream.cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream[B]())((h, t) => f(h).append(t))

  // ex 5.13
  // implement in terms of unfold
  def mapU[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }

  def takeU(n: Int): Stream[A] =
    Stream.unfold((this, n)) {
      case (_, 0) => None
      case (Empty, _) => None
      case (Cons(h, t), i) => Some((h(), (t(), i - 1)))
    }

  def takeWhileU(pred: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Empty => None
      case Cons(h, t) => {
        val _h = h()
        if (pred(_h)) Some((_h, t()))
        else None
      }
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(s1h, s1t), Cons(s2h, s2t)) => Some((f(s1h(), s2h()), (s1t(), s2t())))
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    def tailOrEmpty[C](s: Stream[C]): Stream[C] = s match {
      case Empty => Stream[C]()
      case Cons(_, t) => t()
    }

    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (_s1, _s2) => Some((
        (_s1.headOption(), _s2.headOption()),
        (tailOrEmpty(_s1), tailOrEmpty(_s2))
      ))
    }
  }

  def startsWith[B>:A](s: Stream[B]): Boolean = {
    zipAll(s).takeWhile(_._2.nonEmpty)
      .forAll(t => t._1 == t._2)
  }

  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Empty => None
      case s@Cons(_, t) => Some((s, t()))
    }.append(Stream(Stream[A]()))

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    foldRight((Stream[B](z), z)) { case (a, (z, acc)) => {
      val v = f(a, acc)
      (Stream.cons(v, z), v)
    }}._1
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  // ex 5.8
  def const[A](v: A): Stream[A] = {
    lazy val z: Stream[A] = Stream.cons(v, z)
    z
  }

  // ex 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def repeat[A](n: Int, v: A): Stream[A] = const(v).take(n)

  // ex 5.11
  def unfold_bad[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def go(x: S, stream: Stream[A]): Stream[A] =
      f(x).fold(stream) { case (a, s) => go(s, stream.append(Stream(a))) }

    go(z, Stream())
  }

  // cheating
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).fold(Stream[A]()) { case (h, s) =>
      Stream.cons(h, unfold(s)(f))
    }
  }

  // ex 5.12
  def fromU(n: Int): Stream[Int] = Stream.unfold(n)(x => Some((x, x + 1)))

  // ex 5.12
  def constU[A](v: A): Stream[A] = Stream.unfold(v)(x => Some((x, x)))


  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val h = hd
    lazy val t = tl
    Cons(() => h, () => tl)
  }

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) Empty
    else cons(as.head, apply(as.tail: _*))
  }
}

object Main {
  def ex1 = {
    assert(Stream(1, 2, 3, 4).toList() == List(1, 2, 3, 4))
  }

  def ex2 = {
    val xs = Stream(1, 2, 3)
    assert(xs.take(1).toList() == List(1))
    assert(xs.take(2).toList() == List(1, 2))
    assert(xs.drop(1).toList() == List(2, 3))
  }

  def ex3 = {
    val xs = Stream(1, 2, 3, 4)
    assert(xs.takeWhile(_ < 3).toList() == List(1, 2))
    assert(xs.takeWhile(_ > 3).toList() == List())
    assert(Stream[Int]().takeWhile(_ > 3).toList() == List())
  }

  def ex4 = {

    val x1 = Stream(1, 2, 3)

    assert(x1.forAll(_ <= 3))
    assert(x1.forAll(_ < 3) == false)

    var last = Int.MinValue
    // we use some mutable state to mark the last seen value
    val p: Int => Boolean = i => {
      last = i
      i == 1
    }

    assert(x1.forAll(p) == false)
    assert(last == 2)


    assert(x1.forAll_(_ <= 3))
    assert(x1.forAll_(_ < 3) == false)
  }

  def ex5 = {
    val xs = Stream(1, 2, 3, 4)
    assert(xs.takeWhile_(_ < 3).toList() == List(1, 2))
    assert(xs.takeWhile_(_ > 3).toList() == List())
    assert(Stream[Int]().takeWhile_(_ > 3).toList() == List())
  }

  def ex6 = {

    val xs = Stream(1, 2)

    assert(xs.headOption() == Option(1))
  }

  def ex7 = {

    // map
    {
      val xs = Stream(1, 2, 3).map(_ + 1)
      assert(xs.toList() == List(2, 3, 4))
    }

    // filter
    {
      val xs = Stream(1, 2, 3).filter(_ > 2)
      assert(xs.toList() == List(3))
    }

    // append
    {
      val xs = Stream(1, 2, 3).append(Stream(4, 5, 6))
      assert(xs.toList() == List(1, 2, 3, 4, 5, 6))
    }

    // flatMap
    {
      val xs = Stream(2, 3).flatMap(i => Stream(i, i))
      assert(xs.toList() == List(2, 2, 3, 3))
    }
  }

  def ex8 = {
    val xs = Stream.const(0).take(5)
    assert(xs.toList() == List(0, 0, 0, 0, 0))
  }

  def ex9 = {
    val xs = Stream.from(0).take(5)
    assert(xs.toList() == List(0, 1, 2, 3, 4))
  }

  // ex 5.10
  def fibs: Stream[Int] = {
    def f(a: Int, b: Int): Stream[Int] = Stream.cons(a, f(b, a + b))
    f(0, 1)
  }

  def ex10 = {
    val xs = fibs.take(10)
    assert(xs.toList() == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
  }

  def ex11 = {
    val xs = Stream.unfold(5)(v => if (v >= 0) Some((v, v - 1)) else None)
    assert(xs.toList() == List(5, 4, 3, 2, 1, 0))
  }

  // ex 5.12
  def fibsU: Stream[Int] = Stream.unfold((0, 1)) { case (a, b) => Some(a, (b, a + b)) }

  // ex 5.12
  def onesU: Stream[Int] = Stream.unfold(1) { _ => Some((1, 1)) }

  def ex12 = {

    // fibs
    {
      val xs = fibsU.take(10)
      assert(xs.toList() == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
    }

    // from
    {
      val xs = Stream.fromU(0).take(5)
      assert(xs.toList() == List(0, 1, 2, 3, 4))
    }
    // const
    {
      val xs = Stream.constU(0).take(5)
      assert(xs.toList() == List(0, 0, 0, 0, 0))
    }
    // ones
    {
      assert(onesU.take(10).toList() == Stream.const(1).take(10).toList())
    }
  }

  def ex13 = {

    // map
    {
      val xs = Stream(1, 2, 3).mapU(_ + 1)
      assert(xs.toList() == List(2, 3, 4))
    }

    // take
    {
      val xs = Stream(1, 2, 3)
      assert(xs.takeU(1).toList() == List(1))
      assert(xs.takeU(2).toList() == List(1, 2))
    }

    // takeWhile
    {
      val xs = Stream(1, 2, 3, 4)
      assert(xs.takeWhileU(_ < 3).toList() == List(1, 2))
      assert(xs.takeWhileU(_ > 3).toList() == List())
      assert(Stream[Int]().takeWhileU(_ > 3).toList() == List())
    }

    // zipWith
    {
      val xs = Stream.from(0)
      val ys = Stream.from(5)

      assert(xs        .zipWith(ys        )((_, _)).take(2).toList() == List((0, 5), (1, 6)))
      assert(xs        .zipWith(ys.take(2))((_, _))        .toList() == List((0, 5), (1, 6)))
      assert(xs.take(2).zipWith(ys        )((_, _))        .toList() == List((0, 5), (1, 6)))
    }

    // zipAll
    {
      val xs = Stream.from(0)
      val ys = Stream.from(5)

      assert(xs        .zipAll(ys        ).take(2).toList() == List((Some(0), Some(5)), (Some(1), Some(6))))
      assert(xs        .zipAll(ys.take(2)).take(3).toList() == List((Some(0), Some(5)), (Some(1), Some(6)), (Some(2), None)))
      assert(xs.take(2).zipAll(ys        ).take(3).toList() == List((Some(0), Some(5)), (Some(1), Some(6)), (None   , Some(7))))
    }
  }

  def ex14 = {
    val xs = Stream.from(0)
    assert(xs.startsWith(Stream.from(0).take(50)))
  }

  def ex15: Unit = {
    val xs = Stream(1, 2, 3)
    assert(xs.tails.map(_.toList()).toList() == List(
      List(1, 2, 3),
      List(2, 3),
      List(3),
      List()
    ))
  }

  def ex16: Unit = {
    val xs = Stream(1, 2, 3).scanRight(0)(_ + _)

    println(xs.toList())
    assert(xs.toList() == List(6, 5, 3, 0))
  }

  def main(args: Array[String]): Unit = {
    ex1
    ex2
    ex3
    ex4
    ex5
    ex6
    ex7
    ex8
    ex9
    ex10
    ex11
    ex12
    ex13
    ex14
    ex15
    ex16

    println("done")
  }
}