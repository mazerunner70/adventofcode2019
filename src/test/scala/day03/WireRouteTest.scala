package day03

import org.scalatest.FlatSpec

class WireRouteTest extends FlatSpec {

  behavior of "WireRouteTest"

  it should "shortestIntersection1" in {
    val routeStringA = "R75,D30,R83,U83,L12,D49,R71,U7,L72"
    val routeStringB = "U62,R66,U55,R34,D71,R55,D58,R83"
    val wireRoute = new WireRoute()
    assert(wireRoute.shortestIntersection(routeStringA, routeStringB) == 159)
  }
  it should "shortestIntersection2" in {
    val routeStringA = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
    val routeStringB = "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
    val wireRoute = new WireRoute()
    assert(wireRoute.shortestIntersection(routeStringA, routeStringB) == 135)
  }

}
