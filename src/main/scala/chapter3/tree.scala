package chapter3

/**
 * Created by fredh on 22-03-2015.
 */

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def max(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => max(l).max(max(r))
  }

  def depth(t: Tree[Int]): Int = t match {
    case Leaf(x) => 0
    case Branch(l, r) => 1 + depth(l).max(depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeByFold(t: Tree[Int]): Int = fold(t)(a => 1)(1 + _ + _)
  def maxByFold(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)
  def depthByFold(t: Tree[Int]): Int = fold(t)(a => 0)((a, b) => 1 + (a max b))
  def mapByFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(x => Leaf(f(x)):Tree[B])(Branch(_, _))
}

object trees {
  import chapter3.Tree._

  val aTree =
    Branch(
      Branch(
        Leaf(1),
        Branch(
          Leaf(2),
          Branch(
            Leaf(3),
            Branch(
              Branch(
                Leaf(7),
                Leaf(6)
              ),
              Leaf(4)
            )
          )
        )
      ),
      Leaf(5)
    )

  /**
   *        x
   *       /\
   *      x 5
   *     /\
   *    1 x
   *     /\
   *    2 x
   *     /\
   *    3 x
   *     /|
   *    x 4
   *   /\
   *  7 6
   */


  def ex25 = {
    assert(size(aTree) == 13)
  }

  def ex26 = {
    assert(max(aTree) == 7)
  }

  def ex27 = {
    assert(depth(aTree) == 6)
  }

  def ex28 = {
    assert(max(map(aTree)(_ * 2)) == max(aTree) * 2)
  }

  def ex29 = {
    assert(size(aTree) == sizeByFold(aTree))
    assert(max(aTree) == maxByFold(aTree))
    assert(depth(aTree) == depthByFold(aTree))
    assert(map(aTree)(_ * 2) == mapByFold(aTree)(_ * 2))
  }

  def main(args: Array[String]) {
    ex25; ex26; ex27; ex28; ex29

    println("done")
  }

}

