package day07

import scala.collection.mutable.ArrayBuffer

abstract class MtProcess {
  def run():Unit // Initial launch point for a process. Return back when either pausing or ending
  def checkCanRun(): Boolean // Used by process to check if it can resume
}

class PreemptiveMultiTasker {

  var processes: ArrayBuffer[MtProcess] = ArrayBuffer()
  var processId = 0

  def addProcess(process: MtProcess): Unit =
    processes = processes += process

  def removeProcess(process: MtProcess) =
    processes = processes -= process

  def run() = {
    def runNextProcess(): Unit = {
      processes.find(_.checkCanRun()) match {
        case Some(process) => {
          process.run()
          runNextProcess()
        }
        case None          =>
      }
    }
    runNextProcess()
  }

}

object PreemptiveMultiTasker {
  def main(args: Array[String]): Unit = {
    val preemptiveMultiTasker = new PreemptiveMultiTasker

  }
}