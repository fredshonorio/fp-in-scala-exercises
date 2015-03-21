package chapter2

import scala.annotation.tailrec

/**
 *
 * Created by fredh on 21-03-2015.
 */
object ex2 {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean) : Boolean =  {

    @tailrec
    def go(p: Int): Boolean = {
      if (p >= as.length) true
      else if (!ordered(as(p - 1), as(p))) false
      else go(p + 1)
    }

    if (as.length == 0 || as.length == 1)  true
    else go(1)

  }

  def strictly_ascending(a: Int, b: Int) : Boolean = { a < b }
  def non_descending(a: Int, b: Int) : Boolean = { a <= b }
  def descending(a: Int, b: Int) : Boolean = { a > b }
  def evens_first(a: Int, b: Int): Boolean = {
    if (a % 2 == b % 2) non_descending(a, b)
    else a % 2 == 0
  }

  def main(args: Array[String]) {

    assert(isSorted(Array(1), non_descending))
    assert(isSorted(Array(1, 2, 2, 3, 60, 61), non_descending))
    assert(isSorted((1 to 100).toArray, strictly_ascending))
    assert(isSorted((1 to 100).toArray, strictly_ascending))
    assert(isSorted(Array(2, 4, 100, 5, 7), evens_first))

    print("done")
  }
}
