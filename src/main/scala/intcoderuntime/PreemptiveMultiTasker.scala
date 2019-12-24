package intcoderuntime

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

abstract class MtProcess {
  def getConsole(): Console
  def run():Unit // Initial launch point for a process. Return back when either pausing or ending
  def checkCanRun(): Boolean // Used by process to check if it can resume
  def isEnded() : Boolean
  def getMemory(): Memory
}

class PreemptiveMultiTasker(val processes: List[MtProcess]) {

  var processId = 0

  def getConsole(processCount: Int = 0): Console = processes.drop(processCount).head.getConsole()

  def isEnded(): Boolean = processes.forall(_.isEnded())

  def run(): Boolean = {
    @tailrec
    def runNextProcess(): Boolean = {
      processes.find(_.checkCanRun()) match {
        case None          => isEnded()
        case Some(process) => {
          process.run()
          runNextProcess()
        }
      }
    }
    runNextProcess()
  }

}

object PreemptiveMultiTasker {
  def apply(memoryArray: ArrayBuffer[Long], processCount: Int = 1): PreemptiveMultiTasker = {
    def wireProcess(): IntCodeInterpreter = {
      val memory = new Memory(memoryArray.clone())
      val console = new Console
      val intCodeInterpreter = new IntCodeInterpreter(memory)
      intCodeInterpreter.console = console
      intCodeInterpreter
    }
    new PreemptiveMultiTasker(List.fill(processCount) (wireProcess))
  }
}
