package day05

import scala.io.Source

class IntCodeInterpreterV2(memory: Array[Int], terminal: Terminal) {

  var programCounter = 0


  def readAndAdvance(): Int = {
    programCounter += 1
    memory(programCounter-1)
  }

  def readAndAdvance(count: Int): List[Int] = {
    programCounter += count
    memory.slice(programCounter-count, programCounter).toList
  }


  def readNextInstruction(): ((List[Char], List[Int])=>Int, List[Char], List[Int]) = {
    val paramCounts = Map(1-> 3, 2-> 3, 3 -> 1, 4 -> 1, 5-> 2, 6-> 2, 7 -> 3, 8 -> 3, 99 -> 0)
    val instructionSet: Map[Int, (List[Char], List[Int]) => Int] = Map(
      1  -> instructionAdd,
      2  -> instructionMultiply,
      3  -> instructionInput,
      4  -> instructionOutput,
      5  -> instructionJumpIfTrue,
      6  -> instructionJumpIfFalse,
      7  -> instructionLessThan,
      8 -> instructionEquals,
      99 -> instructionEnd)

    val opCode = readAndAdvance
    val paramCount = paramCounts(opCode % 100)
    val parameterModes: List[Char] = (opCode.toString.reverse+("0" * (paramCount+2))).substring(2).toList
    val params = readAndAdvance(paramCount)
    println(s"debug: $opCode:$paramCount:$params")
    (instructionSet(opCode % 100), parameterModes, params)
  }

  def parameterValue(mode: Char, param: Int): Int = mode match {
    case '0' => memory(param)
    case '1' => param
  }

  def doMath(params: List[Int], parameterModes: List[Char], operation: (Int, Int)=>Int) = {
    val valueA = parameterValue(parameterModes.head, params.head)
    val valueB = parameterValue(parameterModes.tail.head, params.tail.head)
    memory(params.last) = operation(valueA, valueB)
    0 // instruction successful
  }

  def instructionAdd(parameterModes: List[Char], params: List[Int]):Int = {
    def operation = (x: Int, y: Int) => x + y
    doMath(params, parameterModes, operation)
  }
  def instructionMultiply(parameterModes: List[Char], params: List[Int]):Int = {
    def operation = (x: Int, y: Int) => x * y
    doMath(params, parameterModes, operation)
  }
  def instructionInput(parameterModes: List[Char], params: List[Int]):Int = {
    val input = terminal.readInt()
    memory(params.head) = input
    0
  }
  def instructionOutput(parameterModes: List[Char], params: List[Int]):Int = {
    val output = memory(params.head)
    terminal.writeInt(output)
    0
  }
  def instructionJumpIfTrue(parameterModes: List[Char], params: List[Int]):Int = {
    val valueA = parameterValue(parameterModes.head, params.head)
    val location = parameterValue(parameterModes.tail.head, params.tail.head)
    if (valueA != 0) programCounter = location
    0
  }
  def instructionJumpIfFalse(parameterModes: List[Char], params: List[Int]):Int = {
    val valueA = parameterValue(parameterModes.head, params.head)
    val location = parameterValue(parameterModes.tail.head, params.tail.head)
    if (valueA == 0) programCounter = location
    0
  }
  def instructionLessThan(parameterModes: List[Char], params: List[Int]):Int = {
    val valueA = parameterValue(parameterModes.head, params.head)
    val valueB = parameterValue(parameterModes.tail.head, params.tail.head)
    val location = params.tail.tail.head
    memory(location) = if (valueA < valueB) 1 else 0
    0
  }
  def instructionEquals(parameterModes: List[Char], params: List[Int]):Int = {
    val valueA = parameterValue(parameterModes.head, params.head)
    val valueB = parameterValue(parameterModes.tail.head, params.tail.head)
    val location = params.tail.tail.head
    memory(location) = if (valueA ==valueB) 1 else 0
    0
  }
  def instructionEnd(parameterModes: List[Char], params: List[Int]):Int = 1

  def runProgram() = {
    var result = 0
    while (result == 0)
    {
      val instruction = readNextInstruction
      result = instruction._1(instruction._2, instruction._3)
    }
  }



}

object IntCodeInterpreterV2 {
  def loadFromFile(filename:String): String = {
    Source.fromResource(filename).mkString
  }

  def main(args: Array[String]): Unit = {
    val pageContent = loadFromFile("day05/input.txt")
    val array = pageContent.split(',').map(_.toInt).toArray
    val terminal1 = new TerminalImpl(1)
    runVm(array, terminal1)
    println(s"Day05 answer part 1 ${terminal1.lastOutput}")
    val terminal2 = new TerminalImpl(5)
    runVm(array, terminal2)
    println(s"Day05 answer part 2 ${terminal2.lastOutput}")

  }

  def day2DoubleCheck(): Unit = {
    println(s"Started")
    val pageContent = loadFromFile("day02/input.txt")
    val array = pageContent.split(',').map(_.toInt).toArray
    val terminal = new TerminalImpl(1)
    val output = runVm(array, terminal, 12, 2)
    println(s"Day02 answer double check: $output")
  }

  def runVm(memory: Array[Int], terminal: Terminal, noun: Int, verb: Int): Int = {
    val memClone = memory.clone()
    memClone(1) = noun
    memClone(2) = verb
    val intCodeInterpreterV2 = new IntCodeInterpreterV2(memClone, terminal)
    intCodeInterpreterV2.runProgram()
    memClone(0)
  }

  def runVm(memory: Array[Int], terminal: Terminal): Unit = {
    val memClone = memory.clone()
    val intCodeInterpreterV2 = new IntCodeInterpreterV2(memClone, terminal)
    intCodeInterpreterV2.runProgram()

  }


}