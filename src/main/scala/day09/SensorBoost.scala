package day09

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class SensorBoost {

  def runOneProcess(memoryArray: ArrayBuffer[Long]): (PreemptiveMultiTasker, Console) = {
    val memory = new Memory(memoryArray.clone())
    val console = new Console
    val intCodeInterpreterV4 = new IntCodeInterpreterV4(memory)
    intCodeInterpreterV4.console = console
    val preemptiveMultiTasker = new PreemptiveMultiTasker
    preemptiveMultiTasker.addProcess(intCodeInterpreterV4)
    (preemptiveMultiTasker, console)
  }

}

object SensorBoost {
  def loadFromFile(filename:String): String = {
    Source.fromResource(filename).mkString
  }

  def main(args: Array[String]): Unit = {
    val pageContent = loadFromFile("day09/input.txt")
    val memoryArray = pageContent.split(',').map(_.toLong).toArray
    val sensorBoost = new SensorBoost
    val memory = memoryArray.to[ArrayBuffer]
    val (preemptiveMultiTasker, console) = sensorBoost.runOneProcess(memory)
    console.inputList.enqueue(1)
    preemptiveMultiTasker.run()
    println(s"day 09 part 1 answer: ${console.lastOutput.getOrElse(-11111)}")
    println(s"day 09 part 2 answer: ${part2(memory)}")
  }

  def part2(memory:ArrayBuffer[Long]): Long = {
    val sensorBoost = new SensorBoost
    val (preemptiveMultiTasker, console) = sensorBoost.runOneProcess(memory)
    console.inputList.enqueue(2)
    preemptiveMultiTasker.run()
    console.lastOutput.getOrElse(-11111)
  }

}