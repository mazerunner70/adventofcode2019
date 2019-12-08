package day06

import org.scalatest.FlatSpec

import scala.io.Source

class OrbitMapTest extends FlatSpec {

  behavior of "OrbitMapTest"

  def loadFromFile(filename:String): String = {
    Source.fromResource(filename).mkString
  }



  it should "orbitCount" in {
    val pageContent = loadFromFile("day06/test-input.txt")
    val listStrings = pageContent.split('\n').toList
    val orbitMap = new OrbitMap
    val entityGraph = orbitMap.createGraph(listStrings)
    val size = orbitMap.orbitCount(entityGraph)
    assert(size == 42)
  }

  it should "orbitCount2" in {
    val pageContent = loadFromFile("day06/test-input-2.txt")
    val listStrings = pageContent.split('\n').toList
    val orbitMap = new OrbitMap
    val entityGraph = orbitMap.createGraph(listStrings)
    val fromEntity = entityGraph("YOU")
    val toEntity = entityGraph("SAN")
    val distance = orbitMap.countTransfers(fromEntity, toEntity)
    assert(distance == 4)
  }

}
