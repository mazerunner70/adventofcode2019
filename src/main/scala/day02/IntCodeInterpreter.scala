package day02

import scala.io.Source


class IntCodeInterpreter(memory: Array[Int]) {

  var programCounter = 0

  def readAdvance(): (Int, Int, Int, Int) = {
    programCounter += 4
    ( memory(programCounter-4),
      memory(programCounter-3),
      memory(programCounter-2),
      memory(programCounter-1) )
  }

  def runProgram() = {
    while (!isProgramEnded()) {
      executeCommand(readAdvance())
    }
  }

  def isProgramEnded(): Boolean = memory(programCounter) == 99


  def executeCommand(command: (Int, Int,Int, Int)) = {
    val (commandId, locationA, locationB, outputLocation) = command
    val result = commandId match {
      case 1 => memory(locationA) + memory(locationB)
      case 2 => memory(locationA) * memory(locationB)
    }
    memory(outputLocation) = result
  }

}

object IntCodeInterpreter {

  def loadFromFile(filename:String): String = {
    Source.fromResource(filename).mkString
  }

  def main(args: Array[String]): Unit = {
    val pageContent = loadFromFile("day02/input.txt")
    val array = pageContent.split(',').map(_.toInt).toArray
    val output = runVm(array, 12, 2)
    println(s"Part1 answer= $output")
    val settings = recurseTestProgram(array, 0)
    println(s"Part2 answer = $settings")
  }

  def runVm(memory: Array[Int], noun: Int, verb: Int): Int = {
    val memClone = memory.clone()
    memClone(1) = noun
    memClone(2) = verb
    val intCodeInterpreter = new IntCodeInterpreter(memClone)
    intCodeInterpreter.runProgram()
    memClone(0)
  }

  def recurseTestProgram(memory: Array[Int], wordPair: Int): Int = {
    val output = runVm(memory, wordPair / 100, wordPair % 100)
    if (output == 19690720) return wordPair
    if (wordPair == 9999) return -1
    recurseTestProgram(memory, wordPair+1)
  }

}