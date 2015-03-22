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
  // annotate Nil as a list of A

  // ex13
  def foldRightByFoldLeft[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((a, b) => f(b, a))
  }

  // ex13
  // TODO: come back to this later, i don't really get this
  // https://github.com/fpinscala/fpinscala/blob/264a0f59c713e76c5de9e1b059bc770ed072ba86/answers/src/main/scala/fpinscala/datastructures/List.scala#L186
  // https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/datastructures/List.scala#L186
  def foldLeftByFoldRight[A,B](as: List[A], z: B)(f: (B,A) => B): B = {
    foldRight(as, (b: B) => b) ((a, g) => b => g(f(b, a))) (z)
  }

  // ex14
  /** Copy a list iterativelly and add l at the end
   * expand Cons(1, Cons(2,Cons(3,Nil))) - z: Cons(4,Cons(5,Cons(6,Nil)))
   * expand Cons(2, Cons(3,Nil)) - z: Cons(4,Cons(5,Cons(6,Nil)))
   * expand Cons(3, Nil) - z: Cons(4,Cons(5,Cons(6,Nil)))
   */

  def appendR[A](as: List[A], l: List[A]): List[A] = {
    foldRight(as, l)(Cons(_, _))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  // ex15
  def concat[A](ls: List[List[A]]) : List[A] = {
    foldRight(ls, Nil:List[A])(append)
  }

  // ex16
  def add1(as: List[Int]): List[Int] = as match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, add1(t))
  }

  // ex17
  def toString(as: List[Double]): List[String] = as match {
    case Nil => Nil
    case Cons(h, t) => Cons(h.toString, toString(t))
  }

  // ex18
  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  // ex18
  def map_2[A, B](as: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]

    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => buf += f(h); go(t)
    }
    go(as)

    // like python f(*list)
    // expands buf.toList which is a scala.List of Bs into B* or B1, B2, B3 ... Bn
    List(buf.toList: _*)
  }

  // ex19
  def filter[A](as: List[A])(p: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) if p(h) => Cons(h, filter(t)(p))
    case Cons(_, t) => filter(t)(p)
  }

  // ex20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  // ex21
  // create a list of a if p(a) is true, otherwise make an empty list
  def filterFlatMap[A](as: List[A])(p: A => Boolean): List[A] = {
    def in(a: A) = if (p(a)) List(a) else List()
    flatMap(as)(in)
  }

  // ex22
  def sum(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, sum(t1, t2))
  }

  // ex23
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  // ex24
  /**
   * for every head, iterate through the sublist to check if the list headed by head matches
   */

  @tailrec
  def hasSubsequence[A](l: List[A], subl: List[A]): Boolean = {

    // check that l starts with sub
    @tailrec
    def start(as: List[A], sub: List[A]): Boolean = (as, sub) match {
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => start(t1, t2)
      case _ => false
    }

    l match {
      case Nil => Nil == subl
      case _ if start(l, subl) => true
      case Cons(h, t) => hasSubsequence(t, subl)
    }
  }
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

  def ex13 =  {

    val x = foldRightByFoldLeft(List("1", "2", "3"))("#")(_ + _)
    val y = foldRight(List("1", "2", "3"),"#")(_ + _)

    assert(x == y)

    val w = foldLeftByFoldRight(List("1", "2", "3"), "#")(_ + _)
    val z = foldLeft(List("1", "2", "3"),"#")(_ + _)

    assert(w == z)

  }

  def ex14 =  {

    val x = List("1", "2", "3")
    val y = List("4", "5", "6")

    assert(appendR(x,y) == List("1", "2", "3", "4", "5", "6"))
  }

  def ex15 =  {

    val l = List(
      List("1", "2", "3"),
      List("4", "5", "6"),
      List("7"),
      List("8", "9")
    )

    assert(concat(l) == List("1", "2", "3", "4", "5", "6", "7", "8", "9"))
  }

  def ex16 =  {

    val l = List(1, 2, 3)

    assert(add1(l) == List(2, 3, 4))
  }

  def ex17 =  {

    val l = List(1.0, 2.0, 3.0)

    assert(List.toString(l) == List("1.0", "2.0", "3.0"))
  }

  def ex18 =  {

    val l = List(1, 2, 3)
    val l1 = List(1.0, 2.0, 3.0)

    assert(map(l)(_ + 1) == add1(l))
    assert(map(l1)(_.toString) == List.toString(l1))
  }

  def ex19 =  {

    val l = List(1, 2, 3, 4, 5)
    assert(filter(l)(_ % 2 == 0) == List(2, 4))

  }

  def ex20 =  {

    val l = List(1, 2, 3)
    def count(n: Int):List[Int] = List((1 to n).toList: _*)
    assert(flatMap(l)(count) == List(1, 1, 2, 1, 2, 3))

  }

  def ex21 =  {
    val l = List(1, 2, 3, 4, 5)
    assert(filter(l)(_ % 2 == 0) == filterFlatMap(l)(_ % 2 == 0))
  }

  def ex22 =  {
    val l = List(1, 2, 3, 4, 5)
    assert(sum(l, l) == List(2, 4, 6, 8, 10))
  }

  def ex23 =  {
    val l = List(1, 2, 3, 4, 5)
    assert(sum(l, l) == zipWith(l, l)(_ + _))
  }

  def ex24 =  {
    val l = List(1, 2, 3, 4, 5)
    assert(hasSubsequence(l, List(1, 2)))
    assert(hasSubsequence(l, List(1)))
    assert(!hasSubsequence(l, List(1, 3)))
    assert(hasSubsequence(l, List(1, 2, 3)))
    assert(hasSubsequence(l, List(1, 2, 3, 4, 5)))
  }

  def main(args: Array[String]) {
    ex1; ex2; ex3; ex4; ex5; ex6 // TODO: come back to ex7 and 8
    ex9; ex10; ex11; ex12; ex13; ex14; ex15; ex16; ex17; ex18; ex19
    ex20; ex21; ex22; ex23; ex24

    print("done")
  }
}

