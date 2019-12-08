package day06

import scala.io.Source

case class Entity(name:String, parent:Option[Entity]){
  override def equals(that: Any): Boolean =
    that match {
      case that: Entity => (name == that.name && parent == that.parent)
      case _            => false
    }
}

class OrbitMap {

  def parseMapData(datum: String): (String, String) = {
    val entityNames = datum.split(')')
    (entityNames(0), entityNames(1))
  }

  def createGraph(mapData: List[String]): Map[String, Entity] = {
    val map: collection.mutable.Map[String, Entity] = collection.mutable.Map("COM" -> Entity("COM", None))
    var loop = true
    while(loop) {
      loop = false
      for (datum <- mapData) {
        val (parent, body) = parseMapData(datum)
        if (!map.contains(parent)) loop = true
        else
          map.update(body, Entity(body, Some(map(parent))))
      }
    }
    map.toMap
  }

  def getGraphList(graphMap: Map[String, Entity]): List[Entity] =
    graphMap.values.toList

  def calculateMapSize(entities:List[Entity]):Int = {
    def getDepth(entity: Entity): Int = {
      entity.parent match {
        case Some(entity) => 1 + getDepth(entity)
        case None         => 0
      }
    }
    val depth = getDepth(entities.head)
    val tail = entities.tail
    tail match {
      case Nil => depth
      case _ => depth + calculateMapSize(tail)
    }
  }

  def orbitCount(orbitMap: Map[String, Entity]): Int = {
    calculateMapSize(getGraphList(orbitMap))
  }

  def orbitList(entity: Entity): List[Entity] = entity.parent match {
    case None         => entity :: Nil
    case Some(parent) => entity :: orbitList(parent)
  }

  def countTransfers(from: Entity, to: Entity): Int = {
    val fromList = orbitList(from)
    val toList = orbitList(to)
    val intersection = fromList.intersect(toList)
    fromList.length + toList.length - intersection.length * 2 - 2
  }
}

object OrbitMap {
  def loadFromFile(filename:String): String = {
    Source.fromResource(filename).mkString
  }

  def main(args: Array[String]): Unit = {
    val pageContent = loadFromFile("day06/input.txt")
    val listStrings = pageContent.split('\n').toList
    val orbitMap = new OrbitMap
    val entityGraph = orbitMap.createGraph(listStrings)
    val size = orbitMap.orbitCount(entityGraph)
    println(s"Day 06 part 1 answer: $size")
    val fromEntity = entityGraph("YOU")
    val toEntity = entityGraph("SAN")
    val distance = orbitMap.countTransfers(fromEntity, toEntity)
    println(s"Day 06 part 2: Transfer distance: $distance")
  }
}