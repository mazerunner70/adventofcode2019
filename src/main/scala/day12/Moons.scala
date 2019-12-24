package day12

import common.SourceAware

import math.signum
import scala.annotation.tailrec
import scala.util.matching.Regex

case class MoonData(positions: List[(Int, Int, Int)], velocities: List[(Int, Int, Int)])

class Moons {
  implicit class TupleAdd(t: (Int, Int, Int)) {
    def +(p: (Int, Int, Int)) = (p._1 + t._1, p._2 + t._2, p._3 + t._3)
  }

  def perMoon(position: (Int, Int, Int), moons: List[(Int, Int, Int)]): (Int, Int, Int) =
    moons match {
      case moon :: _ => (signum(moon._1 - position._1), signum(moon._2 - position._2),
        signum(moon._3 - position._3)) + perMoon(position, moons.tail)
      case Nil => (0,0,0)
    }

  def applyGravity(positions: List[(Int, Int, Int)], all_positions: List[(Int, Int, Int)]): List[(Int, Int, Int)] = {
    positions match {
      case position :: _ => perMoon(position, all_positions) :: applyGravity(positions.tail, all_positions)
      case Nil           => Nil
    }
  }

  def calc(moons: MoonData): MoonData = {
    val velocityDeltas = applyGravity(moons.positions, moons.positions)
    val newVelocities = moons.velocities.zip(velocityDeltas).map { case ((x, y)) => x + y}.toList
    val newPositions = moons.positions.zip(newVelocities). map { case ((x, y)) => x + y}.toList
    MoonData(newPositions, newVelocities)
  }

  def iterate(count: Int, moonData: MoonData) : MoonData = {
    if (count>0) {
      iterate(count-1, calc(moonData))
    } else
      moonData
  }

  @tailrec
  final def iterateUntil(count: Long, moonData: MoonData, startMoonData:MoonData) : Long = {
    val newMoonData = calc(moonData)
    if (count / 10000000 == count % 10000000) println(s"${count/1000000}M")
    if (newMoonData == startMoonData) count
    else
      iterateUntil(count+1, newMoonData, startMoonData)
  }

  def parseConfig(moonsConfig: List[String]): List[(Int, Int, Int)] = {
    moonsConfig match {
      case moon :: _ =>  {
        val pattern:Regex = "<x=([^,]+), y=([^,]+), z=([^,]+)>".r
        val pattern(x, y, z) = moonsConfig.head
        (x.toInt, y.toInt, z.toInt) ::parseConfig(moonsConfig.tail)
      }
      case Nil       => Nil
    }
  }

  def energyLevel(moonData: MoonData): Int = {
    List(moonData.positions.map(x => math.abs(x._1)+math.abs(x._2)+math.abs(x._3)).toList,
      moonData.velocities.map(x => math.abs(x._1)+math.abs(x._2)+math.abs(x._3)).toList).transpose.map(_.product).sum
  }

}

object Moons {
  def main(args: Array[String]): Unit = {
    val contents = SourceAware.loadFromFile("day12/input.txt")
    val lines = contents.split('\n').toList
    val moons = new Moons()
    val positions = moons.parseConfig(lines)
    var moonData = MoonData(positions, List.fill(positions.size)((0,0,0)))
    moonData = moons.iterate(1000, moonData)
    val energy = moons.energyLevel(moonData)
    println(s"day12 part 1 answer: $energy")

    var moonData2 = MoonData(positions, List.fill(positions.size)((0,0,0)))
    val count = moons.iterateUntil(0, moonData2, moonData2)
    println(s"day12 part 2 answer: $count")

  }
}
