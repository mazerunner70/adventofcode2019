package day07

import day05.Terminal

import scala.collection.mutable

class MtTerminalImpl extends Terminal{

  val inputList: mutable.Queue[Int] = mutable.Queue()
  var writesTo : MtTerminalImpl = _
  var lastOutput = 0

  override def readInt(): Int = inputList.dequeue()

  override def writeInt(value: Int): Unit = {
    writesTo.inputList.enqueue(value)
    lastOutput = value
  }
}
