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
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
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

  def main(args: Array[String]): Unit = {
    ex1
    ex2
    ex3
    ex4
    ex5
    ex6

    println("done")
  }
}