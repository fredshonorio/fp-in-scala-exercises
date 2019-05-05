package chapter6

import chapter6.random.{Rand, SimpleRNG}
import org.scalatest.FunSuite

class randomTest extends FunSuite {
  test("basic") {
    assert(SimpleRNG(42).nextInt._1 == 16159453)
  }

  // the following tests are not really satisfactory

  private val rng1 = SimpleRNG(1)

  test("pos") {
    assert(rng1.nonNegativeInt._1 >= 0)
  }

  test("double") {
    val v = rng1.double._1
    assert(v >= 0d && v < 1)
  }

  test("ints") {
    val v = rng1.ints(5)._1
    assert(v.distinct.size == 5)
  }

  test("double by map") {
    assert(Rand.double(rng1)._1 == rng1.double._1)
  }

  test("map2") {
    assert(
      Rand.map2(Rand.double, Rand.int)((_, _))(rng1)._1 ==
      (rng1.double._1, rng1.double._2.nextInt._1)
    )
  }

  test("sequence") {
    assert(
      Rand.sequence(List(
        Rand.int,
        Rand.map(Rand.int)(_ + 1),
        Rand.map(Rand.int)(_ + 2),
      ))(rng1)._1 == List(
        rng1.nextInt._1,
        rng1.nextInt._2.nextInt._1 + 1,
        rng1.nextInt._2.nextInt._2.nextInt._1 + 2,
      )
    )
  }

  test("ints by sequence") {
    assert(
      Rand.ints(3)(rng1)._1 ==
      List(
        rng1.nextInt._1,
        rng1.nextInt._2.nextInt._1,
        rng1.nextInt._2.nextInt._2.nextInt._1)
    )
  }

  test("non negative less than") {
    assert(Rand.nonNegativeLessThan(3)(rng1)._1 < 3)
  }

  test("map, map2 by flatMap") {
    assert(Rand.map(Rand.int)(_ + 1)(rng1)._1 == Rand.mapFm(Rand.int)(_ + 1)(rng1)._1)
    assert(Rand.map2(Rand.int, Rand.int)(_ + _)(rng1)._1 == Rand.map2Fm(Rand.int, Rand.int)(_ + _)(rng1)._1)
  }
}
