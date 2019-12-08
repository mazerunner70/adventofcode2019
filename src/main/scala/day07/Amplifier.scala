package day07

import day05.{IntCodeInterpreterV2, Terminal}

import scala.io.Source

class TerminalImpl(config: Int, inputSignal: Int) extends Terminal {
  val inputSequence = List(config, inputSignal)
  var ctr = -1
  var lastOutput = 0

  override def readInt(): Int = {
    ctr += 1
    inputSequence(ctr)
  }

  override def writeInt(value: Int): Unit = {
    println(s"Output: $value")
    lastOutput = value
  }
}

class Amplifier {

  def runVm(memory: Array[Int], config: Int, inputSignal: Int): Int = {
    val memClone = memory.clone()
    val terminal = new TerminalImpl(config, inputSignal)
    val intCodeInterpreterV2 = new IntCodeInterpreterV2(memClone, terminal)
    intCodeInterpreterV2.runProgram()
    terminal.lastOutput
  }

  def runAmplifierList(memory: Array[Int], configList: List[Int], inputSignal: Int): Int = {
    configList match {
      case config :: tail => {
        val outputSignal = runVm(memory, config, inputSignal)
        runAmplifierList(memory, tail, outputSignal)
      }
      case Nil            => inputSignal
    }
  }

  def createAllPermutations(list: List[Int]): List[List[Int]] = list.permutations.toList

  def calculateAllThrusts(memory: Array[Int], thrustPerms: List[List[Int]]): List[Int] = {
    thrustPerms match {
      case phaseSetting :: tail => {
        val outputSignal = runAmplifierList(memory, phaseSetting, 0)
        outputSignal :: calculateAllThrusts(memory, tail)
      }
      case Nil                  => Nil
    }
  }

  def doCalculation(memory: Array[Int] ): Int = {
    val phaseSettings = List(0, 1, 2, 3, 4)
    val possiblePermutations = createAllPermutations(phaseSettings)
    calculateAllThrusts(memory, possiblePermutations).max
  }

}

object Amplifier {

  def loadFromFile(filename:String): String = {
    Source.fromResource(filename).mkString
  }

  def main(args: Array[String]): Unit = {
    val pageContent = loadFromFile("day07/input.txt")
    val array = pageContent.split(',').map(_.toInt).toArray
    val amplifier = new Amplifier
    val result = amplifier.doCalculation(array)
    println(s"day 7 part 1 answer: $result")
  }
}

