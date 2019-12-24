package day12

import common.SourceAware
import org.scalatest.FlatSpec

class OneDSimulatorTest extends FlatSpec {

  behavior of "OneDSimulatorTest"

  it should "iterateUntil" in {
    val oneDSimulator = new OneDSimulator()
    val (count, moonData) = oneDSimulator.iterateUntil((List(-1, 2, 4, 3), List(0,0,0,0)))
    assert (count == 18)
  }

  it should "iterate" in {
    val oneDSimulator = new OneDSimulator()
    val moonData = oneDSimulator.iterate(1, (List(-1, 2, 4, 3), List(0,0,0,0)))
    assert (moonData == (List(2, 3, 1, 2), List(3, 1, -3, -1)))
  }
  it should "iterate2" in {
    val oneDSimulator = new OneDSimulator()
    val moonData = oneDSimulator.iterate(10, (List(-1, 2, 4, 3), List(0,0,0,0)))
    assert (moonData == (List(2, 1, 3, 2), List(-3, -1, 3, 1)))
  }
  it should "perMoon" in {
    val oneDSimulator = new OneDSimulator()
    val moonData = oneDSimulator.perMoon(-1, List(-1, 2, 4, 3))
    assert (moonData == 3)
  }
  it should "perMoon2" in {
    val oneDSimulator = new OneDSimulator()
    val moonData = oneDSimulator.perMoon(2, List(-1, 2, 4, 3))
    assert (moonData == 1)
  }

  it should "calPeriods" in {
    val contents = SourceAware.loadFromFile("day12/test-input.txt")
    val lines = contents.split('\n').toList
    val oneDSimulator = new OneDSimulator()
    val periods = oneDSimulator.calcPeriods(lines)
    assert(periods == List(18, 28, 44))
    assert(oneDSimulator.lcm(oneDSimulator.lcm(18, 28), 44) == 2772)
    assert(oneDSimulator.lcm(periods) == 2772)
  }
}
