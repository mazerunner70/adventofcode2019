package intcoderuntime


import intcoderuntime.Mode.Mode

import scala.collection.mutable.ArrayBuffer


object Mode extends Enumeration {
  type Mode = Value
  val RELATIVE, ABSOLUTE = Value
}


class Memory(val memory: ArrayBuffer[Long]) {


  var programCounter = 0
  var relativeBase = 0

  def readAndAdvance(): Long = {
    programCounter += 1
    memory(programCounter-1)
  }

  def readAndAdvance(count: Int): List[Long] = {
    programCounter += count
    memory.slice(programCounter-count, programCounter).toList
  }

  def jumpToAddress(newAddress: Int) = programCounter = newAddress

  def adjustRelativeBase(newRelativeBase: Int) = {
    relativeBase += newRelativeBase
//    println(s"new relative base = $relativeBase")
  }

  def prepareMemory(testLocation: Int) = {
    val excess = testLocation - memory.size+1
    if (excess>0) memory.appendAll(List.fill(excess)(0L))
  }

  def adjustAddress(address: Int, mode:Mode = Mode.ABSOLUTE): Int = {
    mode match {
      case Mode.ABSOLUTE => address
      case Mode.RELATIVE => relativeBase + address
    }
  }

  def peek(address: Int, mode:Mode = Mode.ABSOLUTE): Long = {
    val adjustedAddress = adjustAddress(address, mode)
    prepareMemory(adjustedAddress)
//    println(s"peek address: $adjustedAddress with mode: $mode")
    memory(adjustedAddress)
  }

  def poke(address: Int, value: Long, mode:Mode = Mode.ABSOLUTE) = {
    val adjustedAddress = adjustAddress(address, mode)
    prepareMemory(adjustedAddress)
//    println(s"poke address: $adjustedAddress with value $value, mode: $mode")
    memory(adjustedAddress) = value
  }

}
