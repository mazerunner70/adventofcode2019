package day03

import scala.annotation.tailrec
import scala.io.Source

class WireRoute {

  val directions = Map('U' -> (0, 1), 'D' -> (0, -1), 'L' -> (-1, 0), 'R' -> (1, 0))

  def calcWirePath(routeString: String): List[(Int, Int)] = {
    val routeCommands = routeString.split(',').toList
    val relativePath = calcRelativeRoutePath(routeCommands).flatten
    calculateAbsolutePath(0, 0, relativePath, Nil)
  }

  @tailrec
  final def calculateAbsolutePath(x: Int, y: Int, relativePath: List[(Int, Int)], accum: List[(Int, Int)]): List[(Int, Int)] = {
    if (relativePath.isEmpty) return accum.reverse
    val pathInc = relativePath.head
    val newX = x+pathInc._1
    val newY = y+pathInc._2
    calculateAbsolutePath(newX, newY, relativePath.tail, (newX, newY) :: accum)
  }

  final def calcRelativeRoutePath(routeCommands: List[String]) : List[List[(Int, Int)]] = {
    if (routeCommands.isEmpty) return Nil
    val routeCommand = routeCommands.head
    val directionChar = routeCommand.head
    val magnitude = routeCommand.tail.toInt
    val direction = directions(directionChar)
    List.fill(magnitude)(direction) :: calcRelativeRoutePath(routeCommands.tail)
  }

  def intersectRoutes(routeA: List[(Int, Int)], routeB: List[(Int, Int)]): List[(Int, Int)] = {
    //    for (step <- routeA if (routeB.contains(step)))
    for (step <- routeA if (debugContains(routeB, step)))
      yield step
  }
  var ctr = 0

  def debugContains(routeB: List[(Int, Int)], step: (Int, Int)) = {
    if (ctr % 1000 == 0) println(ctr)
    ctr = ctr + 1
    routeB.contains(step)
  }

  def manhattenDistance(position: (Int, Int)): Int =
    math.abs(position._1) + math.abs(position._2)

  def shortestIntersection(routeStringA: String, routeStringB: String) = {
    val wireAPath = calcWirePath(routeStringA)
    val wireBPath = calcWirePath(routeStringB)
    val intersections = intersectRoutes(wireAPath, wireBPath)
    intersections.map(manhattenDistance).min
  }

}

object WireRoute {
  def loadFromFile(filename:String): String = {
    Source.fromResource(filename).mkString
  }

  def main(args: Array[String]): Unit = {
    val pageContent = loadFromFile("day03/input.txt")
    val routes = pageContent.split('\n').toList
    val wireRoute = new WireRoute()
    val shortestDistance = wireRoute.shortestIntersection(routes(0), routes(1))
    println(s"Day03 part 1 answer = $shortestDistance")
  }
}