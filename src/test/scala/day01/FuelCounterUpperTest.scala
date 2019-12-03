package day01

import org.scalatest.FlatSpec

class FuelCounterUpperTest extends FlatSpec {

  behavior of "FuelCounterUpperTest"

  it should "calculateFuel" in {
    assert(2 == FuelCounterUpper.calculateFuel(12))
    assert(2 == FuelCounterUpper.calculateFuel(14))
    assert(654 == FuelCounterUpper.calculateFuel(1969))
    assert(33583 == FuelCounterUpper.calculateFuel(100756))
  }

  it should "total" in {
    val list = List(12, 14, 1969, 100756)
    assert(2+2+654+33583 == FuelCounterUpper.total(list))
  }

  it should "reciprocalFuel" in {
    assert (FuelCounterUpper.reciprocalFuel(14) == 2)
    assert (FuelCounterUpper.reciprocalFuel(1969) == 966)
    assert (FuelCounterUpper.reciprocalFuel(100756) == 50346)
  }

}
