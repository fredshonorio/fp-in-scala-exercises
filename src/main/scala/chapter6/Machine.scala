package chapter6

sealed trait Lock
case object Locked extends Lock
case object Unlocked extends Lock

object Lock {
  def unlockWhen(c: Boolean): Lock = if (c) Unlocked else Locked
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(lock: Lock, candies: Int, coins: Int)

object Machine {

  def update(i: Input, m: Machine): Machine = {
    (i, m) match {
      // inserting a coin into a locked machine will cause it to unlock if there's any candy left
      case (Coin, Machine(Locked, candies, coins)) =>
        Machine(Lock.unlockWhen(candies > 0), candies, coins + 1)

      // turning the knob on an unlocked machine will cause it to dispense candy and become locked
      case (Turn, Machine(Unlocked, candies, coins)) =>
        Machine(Locked, candies - 1, coins)

      // turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing
      case (Turn, Machine(Locked, _, _)) => m
      case (Coin, Machine(Unlocked, candies, coins)) => Machine(Unlocked, candies, coins + 1)

      // a machine that's out of candy ignores all inputs
      case (_, Machine(_, 0, _)) => m
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- State.sequence(inputs.map(i => State.modify[Machine](m => update(i, m))))
      s <- State.get
    } yield (s.candies, s.coins)

}
