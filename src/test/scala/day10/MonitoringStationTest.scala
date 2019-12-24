package day10

import org.scalatest.FlatSpec

import scala.collection.mutable.ListBuffer
import scala.io.Source

class MonitoringStationTest extends FlatSpec {

  def loadFromFile(filename:String): String = {
    Source.fromResource(filename).mkString
  }



  behavior of "MonitoringStationTest"

  it should "parseMap1" in {
    val pageContent = loadFromFile("day10/test-input-1.txt")
    val map = pageContent.split('\n').toList
    val monitoringStation = new MonitoringStation()
    val result = monitoringStation.parseMap(0, map)
    assert(result == List((1,0), (4,0), (0,2), (1,2), (2,2), (3,2), (4,2), (4,3), (3,4), (4,4)))
  }

  it should "parseMap2" in {
    val pageContent = loadFromFile("day10/test-input-2.txt")
    val map = pageContent.split('\n').toList
    val monitoringStation = new MonitoringStation()
    val result = monitoringStation.parseMap(0, map)
    assert(result == List((6,0), (8,0), (0,1), (3,1), (5,1), (2,2), (3,2), (4,2), (5,2), (6,2), (7,2), (8,2), (1,3), (3,3), (5,3), (6,3), (7,3), (1,4), (4,4), (2,5), (7,5), (9,5), (0,6), (3,6), (8,6), (1,7), (2,7), (4,7), (7,7), (8,7), (9,7), (0,8), (1,8), (5,8), (8,8), (1,9), (6,9), (7,9), (8,9), (9,9)))
  }

  it should "getMaximumVisibility1" in {
    val pageContent = loadFromFile("day10/test-input-1.txt")
    val map = pageContent.split('\n').toList
    val monitoringStation = new MonitoringStation()
    val asteroids = monitoringStation.parseMap(0, map)
    val asteroidVisibility = monitoringStation.analyseAllPoints(asteroids, asteroids)
    val result = monitoringStation.getMaximumVisibility(asteroidVisibility)
    assert(result._1 == (3, 4))
  }

  it should "getMaximumVisibility2" in {
    val pageContent = loadFromFile("day10/test-input-2.txt")
    val map = pageContent.split('\n').toList
    val monitoringStation = new MonitoringStation()
    val asteroids = monitoringStation.parseMap(0, map)
    val asteroidVisibility = monitoringStation.analyseAllPoints(asteroids, asteroids)
    val result = monitoringStation.getMaximumVisibility(asteroidVisibility)
    assert(result == ((5,8), 33))
  }

  it should "getMaximumVisibility3" in {
    val pageContent = loadFromFile("day10/test-input-3.txt")
    val map = pageContent.split('\n').toList
    val monitoringStation = new MonitoringStation()
    val asteroids = monitoringStation.parseMap(0, map)
    val asteroidVisibility = monitoringStation.analyseAllPoints(asteroids, asteroids)
    val result = monitoringStation.getMaximumVisibility(asteroidVisibility)
    assert(result == ((1,2), 35))
  }

  it should "getMaximumVisibility4" in {
    val pageContent = loadFromFile("day10/test-input-4.txt")
    val map = pageContent.split('\n').toList
    val monitoringStation = new MonitoringStation()
    val asteroids = monitoringStation.parseMap(0, map)
    val asteroidVisibility = monitoringStation.analyseAllPoints(asteroids, asteroids)
    val result = monitoringStation.getMaximumVisibility(asteroidVisibility)
    assert(result == ((6,3), 41))
  }

  it should "getMaximumVisibility5" in {
    val pageContent = loadFromFile("day10/test-input-5.txt")
    val map = pageContent.split('\n').toList
    val monitoringStation = new MonitoringStation()
    val asteroids = monitoringStation.parseMap(0, map)
    val asteroidVisibility = monitoringStation.analyseAllPoints(asteroids, asteroids)
    val result = monitoringStation.getMaximumVisibility(asteroidVisibility)
    assert(result == ((11,13), 210))
  }

  it should "asAngle" in {
    for (x <- 0 to 360 by 45) println(math.tan(x.toRadians))
    val monitoringStation = new MonitoringStation()
    assert(monitoringStation.asAngle((0,-10)) == 0)
    assert(monitoringStation.asAngle((10,-10)) == 45)
    assert(monitoringStation.asAngle((10,0)) == 90)
    assert(monitoringStation.asAngle((10,10)) == 135)

    assert(monitoringStation.asAngle((0,10)) == 180)
    assert(monitoringStation.asAngle((-10,10)) == 225)
    assert(monitoringStation.asAngle((-10,0)) == 270)
    assert(monitoringStation.asAngle((-10,-10)) == 315)
  }


  it should "transformToFireOrder" in {
    val pageContent = loadFromFile("day10/test-input-6.txt")
    val map = pageContent.split('\n').toList
    val monitoringStation = new MonitoringStation()
    val asteroids = monitoringStation.parseMap(0, map)
    val point = (8, 3)
    val asteroidLines = monitoringStation.getAsteroidLines(point, asteroids.to[ListBuffer] -point)
    val asAngles = monitoringStation.attachAngle(point, asteroidLines)
    val fireOrder = monitoringStation.transformToFireOrder(point, asAngles)
    assert(fireOrder(0) == (8, 1))
    assert(fireOrder(1) == (9, 0))
    assert(fireOrder(2) == (9, 1))
    assert(fireOrder(3) == (10, 0))
    assert(fireOrder(4) == (9, 2))
    assert(fireOrder(5) == (11, 1))
  }

}
