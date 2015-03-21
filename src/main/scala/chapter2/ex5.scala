package chapter2

import chapter2.ex3.{curry, mul}

/**
 *
 * Created by fredh on 21-03-2015.
 */
object ex5 {

  def compose[A, B, C](f: A => B, g: B => C): A => C = {
    a => g(f(a))
  }

  val mul5 = curry(mul)(5)
  val add5 = (x: Int) => x + 5

  def main(args: Array[String]) {

    assert(compose(add5, mul5)(3) == (3 + 5) * 5)
    assert(compose(mul5, add5)(3) == (3 * 5) + 5)

    print("done")
  }
}
