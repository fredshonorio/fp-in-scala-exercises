package chapter6

import chapter6.Machine.simulateMachine
import org.scalatest.FunSuite

class MachineTest extends FunSuite {

  test("testSimulateMachine") {

    val m = Machine(Locked, 5, 10)

    val (candies, coins) = simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Turn, Turn, Coin, Turn)).run(m)._1

    assert(candies == 1)
    assert(coins == 14)
  }

}
