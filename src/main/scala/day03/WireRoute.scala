package day03

class WireRoute {

  val directions = Map('U' -> (0, 1), 'D' -> (0, -1), 'L' -> (-1, 0), 'R' -> (1, 0))

  def calcWirePath(routeString: String): List[(Int, Int)] = {
    val routeCommands = routeString.split(',').toList
    val relativePath = calcRelativeRoutePath(routeCommands)
    (0, 0) :: calculateAbsolutePath(0, 0, relativePath)
  }

  def calculateAbsolutePath(x: Int, y: Int, relativePath: List[(Int, Int)]): List[(Int, Int)] = {
    if (relativePath.isEmpty) return Nil
    val pathInc = relativePath.head
    val newX = x+pathInc._1
    val newY = y+pathInc._2
    (newX, newY) :: calculateAbsolutePath(newX, newY, relativePath.tail)
  }

  def calcRelativeRoutePath(routeCommands: List[String]) : List[(Int, Int)] = {
    if (routeCommands.isEmpty) return Nil
    val routeCommand = routeCommands.head
    val directionChar = routeCommand.head
    val magnitude = routeCommand.tail.toInt
    val direction = directions(directionChar)
    val path = for(distance <- 1 to magnitude)
      yield (direction._1*distance, direction._2*distance)
    path.toList :: calcRelativeRoutePath(routeCommands.tail)
  }

  def intersectRoutes(routeA: List[(Int, Int)], routeB: List[(Int, Int)]): List[(Int, Int)] = {
    for (step <- routeA if (routeB.contains(step)))
      yield step
  }

}
