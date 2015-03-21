package chapter2

import scala.annotation.tailrec

/**
 *
 * Created by fredh on 21-03-2015.
 */
object ex3 {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => {
      (b: B) => {
        f(a, b)
      }
    }
  }

  /**
   * it's curry all the way down!
   *
   * => is right-associative:
   * A => B => C <=> A => (B => C)
   * so the parenthesis are not required
   */
  def curry5[A, B, C, D, E, F](f: (A, B, C, D, E) => F): A => (B => (C => (D => (E => F)))) = {
    // the type is inferred by the signature of f
    a => b => c => d => e => f(a, b, c, d, e)
  }

  def mul(n: Int, m: Int): Int = { n * m }

  def chain_mul(a: Int, b: Int, c: Int, d:Int, e: Int): Int = { a * b * c * d * e}

  def main(args: Array[String]) {

    val c_mul = curry(mul)
    assert(c_mul(4)(3) == mul(4, 3))

    val cc_mul = curry5(chain_mul)
    assert(cc_mul(2)(3)(4)(5)(6) == chain_mul(2, 3, 4, 5, 6))

    print("done")
  }
}
