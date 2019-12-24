package day12

import common.SourceAware
import org.scalatest.FlatSpec

import scala.util.matching.Regex

class MoonsTest extends FlatSpec {

  behavior of "MoonsTest"

  it should "permoon" in {
    val positions = List((-1, 0, 2), (2, -10, -7), (4, -8, 8), (3, 5, -1))
    val moons = new Moons()
    assert(moons.perMoon(positions(0), positions) == (3,-1,-1))
    assert(moons.perMoon(positions(1), positions) == (1, 3, 3))
    assert(moons.perMoon(positions(2), positions) == (-3, 1,-3))
    assert(moons.perMoon(positions(3), positions) == (-1,-3, 1))
  }

  it should "calc" in {
    val contents = SourceAware.loadFromFile("day12/test-input.txt")
    val lines = contents.split('\n').toList
    val moons = new Moons()
    val positions = moons.parseConfig(lines)
    var moonData = MoonData(positions, List.fill(positions.size)((0,0,0)))
    assert(moonData == MoonData(List((-1, 0, 2), (2, -10, -7), (4, -8, 8), (3, 5, -1)), List((0,0,0),(0,0,0),(0,0,0),(0,0,0))))
    moonData = moons.iterate(1, moonData)
    assert(moonData == MoonData(List((2, -1, 1), (3, -7, -4), (1, -7, 5), (2, 2, 0)), List((3,-1,-1),(1,3,3),(-3,1,-3),(-1,-3,1))))
    moonData = moons.iterate(1, moonData)
    assert(moonData == MoonData(List((5, -3,-1), (1, -2, 2), (1, -4, -1), (1,-4, 2)), List((3,-2,-2),(-2,5,6),(0,3,-6),(-1,-6,2))))
    moonData = moons.iterate(8, moonData)
    assert(moonData == MoonData(List((2, 1,-3), (1, -8,0), (3, -6, 1), (2,0, 4)), List((-3,-2,1),(-1,1,3),(3,2,-3),(1,-1,-1))))
    val energy = moons.energyLevel(moonData)
    assert(energy == 179)
  }

  it should "parseConfig" in {
    val contents = SourceAware.loadFromFile("day12/test-input.txt")
    val lines = contents.split('\n').toList
    val moons = new Moons()
    assert(moons.parseConfig(lines) == List())
  }

  it should "regex" in {
    val pattern:Regex = "<x=([^,]+), y=([^,]+), z=([^,]+)>".r
    val pattern(x, y, z) = "<x=-1, y=0, z=2>"
    val tpl = (x.toInt, y.toInt, z.toInt)
    assert(tpl == (-1, 0, 2))
  }

}
