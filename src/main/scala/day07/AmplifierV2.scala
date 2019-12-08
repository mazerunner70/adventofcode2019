package day07

import scala.io.Source

class AmplifierV2 {

  def defineVms(memory: Array[Int], config: List[Int]): List[IntCodeInterpreterV3] = config match {
    case head :: tail => {
      val memClone = memory.clone()
      new IntCodeInterpreterV3(memClone) :: defineVms(memory, tail)
    }
    case Nil          => Nil
  }

  def circularWireTerminals(config: List[Int]): List[MtTerminalImpl] = {
    val terminals = List.tabulate(5)(_ => new MtTerminalImpl())
    def calcAllBarLast(terminals: List[MtTerminalImpl]): Unit = terminals match {
      case head :: Nil => Nil
      case head :: next :: tail => {
        head.writesTo = next
        calcAllBarLast(next :: tail)
      }
    }
    calcAllBarLast(terminals)
    terminals.last.writesTo = terminals.head // Add last circular reference
    terminals
  }

  def applyConfigAsTerminalInput(terminals: List[MtTerminalImpl], config: List[Int]): Unit = config match {
    case Nil =>
    case head :: tail => {
      terminals.head.inputList.enqueue(head)
      applyConfigAsTerminalInput(terminals.tail, tail)
    }
  }

  def attachTerminals(vms: List[IntCodeInterpreterV3], terminals: List[MtTerminalImpl]): Unit = terminals match {
    case Nil =>
    case head :: tail => {
      vms.head.terminal = head
      attachTerminals(vms.tail, tail)
    }
  }

  def constructVmSet(memory: Array[Int], config: List[Int]): List[IntCodeInterpreterV3] = {
    val vms = defineVms(memory, config)
    val terminals = circularWireTerminals(config)
    applyConfigAsTerminalInput(terminals, config)
    attachTerminals(vms, terminals)
    vms
  }

  def attachVmsToMultiTasker(multiTasker: PreemptiveMultiTasker, vms: List[IntCodeInterpreterV3]): Unit = vms match {
    case Nil =>
    case head :: tail => {
      multiTasker.addProcess(head)
      attachVmsToMultiTasker(multiTasker, tail)
    }
  }

  def setInitialInputSignal(signal: Int, vms: List[IntCodeInterpreterV3]) = {
    vms.head.terminal.inputList.enqueue(signal)
  }

}

object AmplifierV2 {
  def loadFromFile(filename:String): String = {
    Source.fromResource(filename).mkString
  }

  def main(args: Array[String]): Unit = {
    val pageContent = loadFromFile("day07/input.txt")
    val memory = pageContent.split(',').map(_.toInt).toArray
    val config = List(9,7,8,5,6)
    val amplifierv2 = new AmplifierV2
    val vms = amplifierv2.constructVmSet(memory, config)
    val multitasker = new PreemptiveMultiTasker
    amplifierv2.attachVmsToMultiTasker(multitasker, vms)
    amplifierv2.setInitialInputSignal(0, vms)
    multitasker.run()

  }
}