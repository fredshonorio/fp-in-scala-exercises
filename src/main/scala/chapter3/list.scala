package chapter3

import scala.annotation.tailrec

/**
 *
 * Created by fredh on 21-03-2015.
 */

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }
  }

  // ex2
  def tail[A](as: List[A]): List[A] = {
    as match {
      case Nil => Nil // this should return option?
      case Cons(_, t) => t
    }
  }

  // ex2
  def tailOpt[A](as: List[A]): Option[List[A]] = {
    as match {
      case Nil => None
      case Cons(_, t) => Some(t)
    }
  }

  // ex3
  def setHead[A](as: List[A], head: A): List[A] = {
    as match {
      case Nil => Nil
      case Cons(_, t) => Cons(head, t)
    }
  }

  // ex4
  def drop[A](as: List[A], n: Int): List[A] = {
    if (n <= 0)
      return as

    as match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  // ex5
  def dropWhile[A](as: List[A], p: A => Boolean) : List[A] = {

    as match {
      case Nil => Nil
      case Cons(h, t) if p(h) => dropWhile(t, p)
      case x => x
    }
  }

  // ex6
  def init[A](as: List[A]) : List[A] = {
    as match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  // ex9
  // receive the value and the accumulator, ignore the value and add 1 for every value
  def length[A](as: List[A]) = foldRight(as, 0)((_, acc) => acc + 1)

  // ex10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  // ex11
  def productL(as: List[Double]) = foldLeft(as, 1d)((acc, x) => acc * x)
  def sumL(as: List[Int]) = foldLeft(as, 0)((acc, x) => acc + x)
  def lengthL[A](as: List[A]) = foldLeft(as, 0)((acc, _) => acc + 1)

  // ex12
  def reverse[A](as: List[A]): List[A] = {

    def go(as: List[A], acc: List[A]) : List[A] =  as match {
      case Nil => acc
      case Cons(h, t) => go(t, Cons(h, acc))
    }

    go(as, Nil)
  }

  // ex12
  def reverseFold[A](as: List[A]): List[A] = foldLeft(as, Nil:List[A])((acc, a) => Cons(a, acc))
  // anotate Nil as a list of A

  // ex13
  // coming up

}

object list {

  import chapter3.List._

  def ex1 = {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // that's it
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    assert(x == 3)
  }

  def ex2 =  {
    val x = List(1, 2, 3, 4)
    val y = Nil

    assert(tail(x) == List(2, 3, 4))
    assert(tailOpt(tailOpt(x).get) == Some(List(3, 4)))
    assert(tailOpt(y) == None)
  }

  def ex3 =  {
    val x = List(1, 2, 3, 4)
    assert(setHead(x, 50) == List(50, 2, 3, 4))
  }

  def ex4 =  {
    val x = List(1, 2, 3, 4)
    assert(drop(x, 1) == List(2, 3, 4))
    assert(drop(x, 2) == List(3, 4))
    assert(drop(x, 3) == List(4))
    assert(drop(x, 4) == Nil)
  }

  def ex5 =  {
    val x:List[Int] = List(1, 2, 3, 4, 5, 6, 7)

    // assert(dropWhile(x, _ < 5) == List(5, 6, 7)) // not sure why this doesn't work
    assert(dropWhile(x, (x: Int) => x < 5) == List(5, 6, 7))
  }

  def ex6 =  {
    val x:List[Int] = List(1, 2, 3, 4, 5, 6, 7)
    assert(init(x) == List(1, 2, 3, 4, 5, 6))
    // getting dumber
  }

  def ex9 =  {
    val x:List[Int] = List(1, 2, 3, 4, 5, 6, 7)
    assert(length(x) == 7)
  }

  def ex10 =  {
    def length2[A](as: List[A]) = foldLeft(as, 0)((acc, _) => acc + 1)

    val x:List[Int] = List(1, 2, 3, 4, 5, 6, 7)

    assert(length2(x) == 7)
  }

  def ex11 =  {

    val x:List[Int] = List(1, 2, 3, 4, 5, 6, 7)
    val y:List[Double] = List(1, 2, 3, 4, 5, 6, 7)

    assert(sumL(x) == sum(x))
    assert(productL(y) == product(y))
    assert(lengthL(x) == 7)
  }

  def ex12 =  {

    val x = List(1, 2, 3)
    val r = List(3, 2, 1)
    assert(reverse(x) == r)
    assert(reverseFold(x) == r)

  }

  def main(args: Array[String]) {
    ex1
    ex2
    ex3
    ex4
    ex5
    ex6
    // TODO: come back to ex7 and 8
    ex9
    ex10
    ex11
    ex12

    print("done")
  }
}
