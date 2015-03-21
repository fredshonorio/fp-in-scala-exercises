package chapter2

import scala.annotation.tailrec

/**
 * Fn = Fn-1 + Fn-2
 *
 * F0 = 0
 * F1 = 1
 *
 * Created by fredh on 21-03-2015.
 */
object ex1 {

  def fib_canonical(n: Int) : Int =  {
    if (n == 0 || n == 1) n
    else fib_canonical(n - 1) + fib_canonical(n - 2)
  }

  def fib(n: Int) : Int =  {

    /**
     * Compute factorial by keeping two accumulation values
     */

    @tailrec
    def _fib(a: Int, b: Int, n: Int): Int = {
      if (n <= 0) a
      else _fib(b, a + b, n - 1)
    }
    _fib(0, 1, n)
  }

  def main(args: Array[String]) {

    (1 to 20) foreach { n =>
      assert(fib(n) == fib_canonical(n))
    }

    print("done")
  }
}
