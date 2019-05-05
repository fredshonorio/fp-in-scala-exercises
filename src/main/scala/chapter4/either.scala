package chapter4

import chapter4.opt.{map2, traverse}

import scala.{Either => _}

trait Either[+E, +A] {
  def map[B](f: A => B) : Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]) : Either[EE, B] // why EE?
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C) : Either[EE, C]
}

case class Left[+E, +A](e: E) extends Either[E, A] {
  override def map[B](f: A => B) = Left(e)
  override def flatMap[EE >: E, B](f: A => Either[EE, B]) = Left(e)
  override def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = b
  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C) = Left(e)
}

case class Right[+E, +A](a: A) extends Either[E, A] {
  override def map[B](f: A => B) = Right(f(a))
  override def flatMap[EE >: E, B](f: A => Either[EE, B]) = f(a)
  override def orElse[EE >: E, B >: A](b: => Either[EE, B]): Right[E, A] = this
  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = b.map(f.apply(a, _))
}

object eithers {

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]) : Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
  }

  def sequence[E, A](es: List[Either[E, A]]) : Either[E, List[A]] =
    traverse(es)(identity)

  def ex6 = {
    val p1 : Either[String, Int] = Left("error")
    val p2 : Either[String, Int] = Right(1).map(_ + 1)
    val p3 : Either[String, Int] = Right(5).map(_ + 1)

    assert(p1.map2(p2)(_ + _) == Left("error"))
    assert(p1.orElse(p2) == Right(2))
    assert(p1.flatMap(_ => p2) == Left("error"))
    assert(p2.flatMap(_ => p3) == Right(6))
    assert(p2.map2(p3)(_ + _) == Right(8))
  }

  def ex7 = {

    val p1 : Either[String, Int] = Left("error")
    val p2 : Either[String, Int] = Right(1).map(_ + 1)
    val p3 : Either[String, Int] = Right(5).map(_ + 1)

    val f : Int => Either[String, Int] = i => if (i == 1) Left("no") else Right(i)

    val xs : List[Int] = List(2, 3, 4)

    assert(sequence(List(p1, p2)) == Left("error"))
    assert(sequence(List(p2, p3)) == Right(List(2, 6)))
    assert(traverse(List(1, 2, 3))(f) == Left("no"))
    assert(traverse(List(2, 3, 4))(f) == Right(List(2, 3, 4)))
  }

  def ex8 = {
    // this would be something like validation
    // Val[E, A]
    // map2 would need to return Val[List[E], A] or maybe be generalizable to accept a f : List[E] => EE that would combine all errors
    // orElse would ignore the error if the `other` is valid, return BOTH errors (?) if both fail
    // traverse, sequence would be like map2, either accumulate errors on a list or accept a merge function
  }

  def main(args: Array[String]): Unit = {
    ex6
    ex7
    ex8

    println("done")
  }

}