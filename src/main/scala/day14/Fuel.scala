package day14

import common.SourceAware

import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex



class Fuel(filename:String) {

  def parseLine(line: String): ((String, Long), List[(String, Long)]) = {
    def getTerm(text:String) : (String, Long) = {val s = text.trim.split(' '); (s(1), s(0).toLong)}
    val matcher = "(\\d+ \\w+[,\\d+ \\w+]*) => (\\d+ \\w+)".r

    val matcher(reactantsText, productText) = line
    val reactants = reactantsText.split(",").map(getTerm(_)).toList
    val product = getTerm(productText)
    (product, reactants)
  }

  def readSource(): List[((String, Long), List[(String, Long)])] = {
    val contents = SourceAware.loadFromFile(filename)
    val lines = contents.split('\n').toList
    lines.map(parseLine(_))
  }

  def mapOfReactions(reactionsList: List[((String, Long), List[(String, Long)])]): Map[String, (Long, List[(String, Long)])] =
    reactionsList.map(r => r._1._1 -> (r._1._2, r._2)).toMap

  def calcReactants(product: String, reactionMap:  Map[String, (Long, List[(String, Long)])], reactantNeeded: mutable.Map[String, Long]): Unit = {
    if (product != "ORE") {
      val reaction = reactionMap(product)
      val productMultiples = reaction._1
      val productNeeded = reactantNeeded(product)
      val batchesNeeded = (productNeeded + productMultiples - 1) / productMultiples //round up optimised for integers
      val mapUpdates = reaction._2.map(reactant => (reactant._1 -> (reactant._2 * batchesNeeded + reactantNeeded.getOrElse(reactant._1, 0L))))
      reactantNeeded ++= mapUpdates
      reactantNeeded += (product -> (reactantNeeded.getOrElse(product, 0L)- batchesNeeded* productMultiples))
//      println(s"making '$product' from ${reactionMap(product)} in $batchesNeeded batches, so far requires:$reactantNeeded")
      reaction._2.foreach(x=>calcReactants(x._1, reactionMap, reactantNeeded))
    }
  }

  def binarySearch(f: (Long => Long), lowBound: Long = 1L, highBound: Long = 0, target: Long = 1000000000000L): Long ={
    val currFuel = (lowBound, highBound) match {
      case (x, 0L) => lowBound * 2
      case (x, y) => (lowBound+highBound)/2
    }
    println(s"Searching: low:$lowBound, high:$highBound, current:$currFuel")
    val oreNeeded = f(currFuel)
    if (highBound > 0 && highBound - lowBound < 2) currFuel
    else
      binarySearch(
        f,
        if (oreNeeded<target) currFuel else lowBound,
        if (oreNeeded>target) currFuel else highBound,
        target
      )
  }

}

object Fuel {
  def main(args: Array[String]): Unit = {
    val fuel = new Fuel("day14/input.txt")
    val reactionList = fuel.readSource()
    val reactionMap = fuel.mapOfReactions(reactionList)
    def testRun(fuelCount: Long): Long = {
      val reactantsNeeded: mutable.Map[String, Long] = mutable.Map("FUEL"->fuelCount)
      fuel.calcReactants("FUEL", reactionMap, reactantsNeeded)
//      println(reactantsNeeded)
      reactantsNeeded("ORE")
    }
    val oreNeeded = testRun(1)
    println(s"Day 14 part 1 answer: ${oreNeeded}")
    val oreNeeded2 = fuel.binarySearch(testRun)
    println(s"part2 answer= $oreNeeded2")
  }
}
