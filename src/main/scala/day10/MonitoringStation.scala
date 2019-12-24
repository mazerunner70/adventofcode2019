package day10

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

class MonitoringStation (){

  // Euclids approach to finding a gcd
  @tailrec
  final def gcd(a: (Int, Int)): Int = if (a._2 == 0) math.abs(a._1) else gcd(a._2, a._1 % a._2)

  final def getAsteroidLines(point: (Int, Int), asteroids: ListBuffer[(Int, Int)]): Map[(Int, Int), ListBuffer[(Int, Int)]] = {
    val asteroidLines = asteroids.groupBy(asteroid => {
      val gcdfactor = gcd(asteroid._1-point._1, asteroid._2-point._2)
      ((asteroid._1-point._1)/gcdfactor, (asteroid._2-point._2)/gcdfactor)
    })
    asteroidLines
  }

  def analyseAllPoints(asteroids: List[(Int, Int)], asteroidsAll: List[(Int, Int)]): List[((Int, Int), Int)] = {
    asteroids match {
      case Nil => Nil
      case asteroid :: _ => {
        val asteroidsVisible = getAsteroidLines(asteroid, asteroidsAll.to[ListBuffer] - asteroid).size
        (asteroid, asteroidsVisible) :: analyseAllPoints(asteroids.tail, asteroidsAll)
      }
    }
  }

  def getMaximumVisibility(asteroidVisibilitys: List[((Int, Int), Int)]): ((Int, Int), Int) = {
    val maxVisibility = asteroidVisibilitys.map(_._2).max
    asteroidVisibilitys.find(_._2 == maxVisibility) match {
      case Some(asteroidVisibility) => asteroidVisibility
      case None                     => throw new Exception
    }
  }

  def parseMap(rowNumber:Int, map: List[String]): List[(Int, Int)] = {
    map match {
      case row :: _ => {
        val coords = row.view.zipWithIndex.flatMap((col) => if (col._1 == '#') Some(col._2, rowNumber) else None).toList
        // Fun line above! means lazy create an array of the chars in the string, alongside their index as tuples
        // Then iterate across creating a coord tuple only if an asteroid symbol is present
        coords ::: parseMap(rowNumber + 1, map.tail)
      }
      case Nil      => Nil
    }
  }
  //angle # atan(y/x)
  def asAngle(increment: (Int, Int)): Double = {
    val y = increment._1.toDouble
    val x = increment._2
    val result = math.atan2(y, -x).toDegrees
    if (result < 0 ) 360+result else result
  }

  def attachAngle(point: (Int, Int), asteroidLines: Map[(Int, Int), ListBuffer[(Int, Int)]]): List[(Double, List[(Int, Int)])] = {
    asteroidLines.map(item => (asAngle(item._1), item._2.toList)).toList
  }

  def transformToFireOrder(dummyPoint: (Int, Int), angledLines: List[(Double, List[(Int, Int)])]):Array[(Int, Int)] = {
    val sorted = angledLines.sortBy(_._1)
    val maxLineLength = sorted.map(_._2.size).max
    val sortedAsteroidLists = sorted.map(_._2.sortBy(x=> math.sqrt(math.pow(x._1-dummyPoint._1, 2)+ math.pow(x._2-dummyPoint._2, 2))))
    val paddedLists = sortedAsteroidLists.map(_.padTo(maxLineLength, dummyPoint))
    val transposed = paddedLists.transpose
    transposed.flatten.filter(_ != dummyPoint).toArray
  }

}

object MonitoringStation {
  def loadFromFile(filename:String): String = {
    Source.fromResource(filename).mkString
  }

  def main(args: Array[String]): Unit = {
    val pageContent = loadFromFile("day10/input.txt")
    val map = pageContent.split('\n').toList
    val monitoringStation = new MonitoringStation()
    val asteroids = monitoringStation.parseMap(0, map)
    val asteroidVisibility = monitoringStation.analyseAllPoints(asteroids, asteroids)
    val result = monitoringStation.getMaximumVisibility(asteroidVisibility)
    println(s"day 10 part 1 answer= $result")

    val asteroidLines = monitoringStation.getAsteroidLines(result._1, asteroids.to[ListBuffer] - result._1)
    val asAngles = monitoringStation.attachAngle(result._1, asteroidLines)
    val fireOrder = monitoringStation.transformToFireOrder(result._1, asAngles)
    println(s"Day10 part 2 answer: ${fireOrder(199)}")
  }
}