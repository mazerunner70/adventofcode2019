package day14

import org.scalatest.FlatSpec

import scala.collection.mutable

class FuelTest extends FlatSpec {

  it should "test1" in {
    val fuel = new Fuel("day14/test-input-3.txt")
    val reactionList = fuel.readSource()
    val reactionMap = fuel.mapOfReactions(reactionList)
    val reactantsNeeded: mutable.Map[String, Long] = mutable.Map("FUEL"->1)
    fuel.calcReactants("FUEL", reactionMap, reactantsNeeded)
    println(reactantsNeeded)
    val oreNeeded = reactantsNeeded("ORE")
    assert(oreNeeded == 9)
  }

  it should "test2" in {
    val fuel = new Fuel("day14/test-input-2.txt")
    val reactionList = fuel.readSource()
    val reactionMap = fuel.mapOfReactions(reactionList)
    val reactantsNeeded: mutable.Map[String, Long] = mutable.Map("FUEL"->1)
    fuel.calcReactants("FUEL", reactionMap, reactantsNeeded)
    println(reactantsNeeded)
    val oreNeeded = reactantsNeeded("ORE")
    assert(oreNeeded == 165)
  }

  it should "test3" in {
    val fuel = new Fuel("day14/test-input-1.txt")
    val reactionList = fuel.readSource()
    val reactionMap = fuel.mapOfReactions(reactionList)
    val reactantsNeeded: mutable.Map[String, Long] = mutable.Map("FUEL"->1)
    fuel.calcReactants("FUEL", reactionMap, reactantsNeeded)
    println(reactantsNeeded)
    val oreNeeded = reactantsNeeded("ORE")
    assert(oreNeeded == 13312)
  }


}
