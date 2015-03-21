package chapter2

import ex3.{curry, mul}

/**
 *
 * Created by fredh on 21-03-2015.
 */
object ex4 {

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def main(args: Array[String]) {

    val mul_ = uncurry(curry(mul))

    assert(mul(4, 3) == mul_(4, 3))
    
    print("done")
  }
}
