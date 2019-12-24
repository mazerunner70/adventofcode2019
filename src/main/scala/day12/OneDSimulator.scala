package day12

import common.SourceAware

import scala.annotation.tailrec
import scala.util.matching.Regex

class OneDSimulator() {

  final def perMoon(position: Int, moons: List[Int]): Int =
    moons.map(x=>math.signum(x - position)).sum

  def applyGravity(positions: List[Int]): List[Int] = {
    positions.map(perMoon(_, positions))
  }

  def calc(moonData: (List[Int],List[Int])): (List[Int], List[Int]) = {
    val velocityDeltas = applyGravity(moonData._1)
    val newVelocities = moonData._2.zip(velocityDeltas).map { case ((x, y)) => x + y}.toList
    val newPositions = moonData._1.zip(newVelocities). map { case ((x, y)) => x + y}.toList
    (newPositions, newVelocities)
  }

  def iterate(count: Int, moonData: (List[Int],List[Int])) :  (List[Int], List[Int]) = {
    var currData = moonData
    for (i <- 1 to count)
      currData = calc(currData)
    currData
  }

  final def iterateUntil(startData: (List[Int],List[Int])) : (Long, (List[Int], List[Int])) = {
    var count = 0L
    var currData = startData
    do {
      currData = calc(currData)
      count += 1
      if (count / 10000000 == count % 10000000) println(s"${count / 1000000}M")
    } while (currData != startData)
    (count, currData)
  }

  def parseConfig(moonsConfig: List[String]): List[List[Int]] = {
    def parseLine(line: String) : List[Int] = {
      val pattern:Regex = "<x=([^,]+), y=([^,]+), z=([^,]+)>".r
      val pattern(x, y, z) = line
      List(x.toInt, y.toInt, z.toInt)
    }
    moonsConfig.map(parseLine(_)).toList
  }


  def energyLevel(moonData: MoonData): Int = {
    List(moonData.positions.map(x => math.abs(x._1)+math.abs(x._2)+math.abs(x._3)).toList,
      moonData.velocities.map(x => math.abs(x._1)+math.abs(x._2)+math.abs(x._3)).toList).transpose.map(_.product).sum
  }

  def calcPeriods(moonsConfig: List[String]) : List[Long]= {
    val dimensionsConfig = parseConfig(moonsConfig). transpose
    dimensionsConfig.map(x=> iterateUntil((x, List.fill(x.size)(0)))._1)
  }

  // https://rosettacode.org/wiki/Least_common_multiple#Scala
  def gcd(a: Long, b: Long):Long=if (b==0) a.abs else gcd(b, a%b)
  def lcm(a: Long, b: Long): Long =(a*b).abs/gcd(a,b)

  def lcm(list: List[Long]): Long = {
    list.reduceLeft(lcm(_, _))
  }

}

object OneDSimulator {
  def main(args: Array[String]): Unit = {
    val contents = SourceAware.loadFromFile("day12/input.txt")
    val lines = contents.split('\n').toList
    val oneDSimulator = new OneDSimulator
    val times = oneDSimulator.calcPeriods(lines)
    println(times)
    println(s"lcm: ${oneDSimulator.lcm(times)}")
  }
}

