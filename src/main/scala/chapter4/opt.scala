package chapter4

/**
 * Created by fredh on 23-03-2015.
 */

import scala.{Option => _, Either => _, Some => _, None => _}

trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}

// ex 4.1
case class Some[+A](it: A) extends Option[A] {
  override def map[B](f: A => B): Option[B] = Some(f(it))
  override def flatMap[B](f: A => Option[B]): Option[B] = f(it)
  override def filter(f: A => Boolean): Option[A] = if (f(it)) Some(it) else None
  override def getOrElse[B >: A](default: => B): B = it
  override def orElse[B >: A](ob: => Option[B]): Option[B] = this
}

// ex 4.1
case object None extends Option[Nothing] {
  override def map[B](f: (Nothing) => B): Option[B] = None
  override def flatMap[B](f: (Nothing) => Option[B]): Option[B] = None
  override def filter(f: (Nothing) => Boolean): Option[Nothing] = None
  override def getOrElse[B >: Nothing](default: => B): B = default
  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob
}


object opt {

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map(f)

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a flatMap (aa => b map (bb => f(aa, bb)))

  def sequence[A](ll: List[Option[A]]): Option[List[A]] = ll match {
    case Nil => Some(Nil)
    case ho :: to => ho.flatMap(h => sequence(to).map(h:: _))
  }

  // in terms of traverse
  def sequence_[A](xs: List[Option[A]]) : Option[List[A]] =
    traverse(xs)(identity)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def ex1 = {
    val v = Some(1)
    assert(v.getOrElse(2) == 1)
    assert(v.orElse(None) == Some(1))
    assert(v.map(_ * 2).getOrElse(None) == 2)
    assert(v.filter(_ == 0).getOrElse("HEY") == "HEY")
    assert(v.flatMap((a) => Some(2 * a)) == Some(2))
  }

  def ex2 = {
    val s = Seq(9, 2, 5, 4, 12, 7, 8, 11, 9, 3, 7, 4, 12, 5, 4, 10, 9, 6, 9, 4).map(_.toDouble)
    assert(variance(s) == Some(8.9))
  }

  def ex3 = {

    val n = Some(2)
    val p = Some(1)

    assert(map2(n, p)(_ + _).getOrElse(throw new Exception()) == 3)
  }

  def pairs(x: Int) = x match {
    case x if x % 2 == 0 => Some(x)
    case _ => None
  }

  def ex4 = {
    val ll = List(1, 2, 3, 4, 5, 6).map(pairs)
    val la = List(1, 2, 3, 4, 5, 6).map(Some(_))
    assert(sequence(ll) == None)
    assert(sequence(la) == Some(List(1, 2, 3, 4, 5, 6)))
  }

  def ex5 = {
    val ll = List(1, 2, 3, 4, 5, 6)
    assert(traverse(ll)(pairs) == None)
    assert(traverse(ll)(Some(_)) == Some(List(1, 2, 3, 4, 5, 6)))

    val lb = List(1, 2, 3, 4, 5, 6).map(pairs)
    val la = List(1, 2, 3, 4, 5, 6).map(Some(_))
    assert(sequence_(lb) == None)
    assert(sequence_(la) == Some(List(1, 2, 3, 4, 5, 6)))
  }

  def main(args: Array[String]) {
    ex1
    ex2
    ex3
    ex4
    ex5

    println("done")
  }
}
