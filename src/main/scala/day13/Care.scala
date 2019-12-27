package day13

import common.SourceAware
import intcoderuntime.PreemptiveMultiTasker

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Care {

  def getResult(vm: PreemptiveMultiTasker): (Map[Long, List[(Long, Long)]], Long) = {
    vm.run()
    val instructions = vm.getConsole().outputQueue.getOrElse(mutable.Queue()).toList.grouped(3).toList
    vm.getConsole().outputQueue.getOrElse(mutable.Queue()).dequeueAll(x=>true)
    val score= instructions.find(i=>i(0) == -1 && i(1) == 0 ).getOrElse(List(0,0,0L))(2)
    val newInstructions = instructions.filterNot(i=>i(0) == -1 && i(1) == 0)
    val instructionSets = newInstructions.groupBy(i => i(2))
    (instructionSets.map(i => i._1->i._2.map(j=> (j(0), j(1))).toList).toMap, score)
   }

  @tailrec
  final def takeTurns(vm: PreemptiveMultiTasker, brickCount: Int, paddlePosition: (Long, Long)): Long = {
    val (tiles, score) = getResult(vm)
    val newBallPosition = tiles.getOrElse(4, List((-1L,-1L))).head
    val newPaddlePosition = tiles.getOrElse(3, List(paddlePosition)).head
    val newBrickCount = if (score == 0) brickCount else brickCount-1
    println(s"----- Ball: $newBallPosition")
    println(s"----Paddle: $newPaddlePosition")
    val joystickAdjust = math.signum(newBallPosition._1 - newPaddlePosition._1)
    vm.getConsole().addToInput(joystickAdjust)
    if (newBallPosition != (-1,-1)) takeTurns(vm, newBrickCount, newPaddlePosition)
    else score
  }

}

object Care {
  def main(args: Array[String]): Unit = {
    val pageContent = SourceAware.loadFromFile("day13/input.txt")
    val memoryArray = pageContent.split(',').map(_.toLong).to[ArrayBuffer]
    val care = new Care()
    val vm = PreemptiveMultiTasker(memoryArray)
    val groups = care.getResult(vm)._1
    for (i <- 0 until 5) {
      println(s"day 13 part 1 answer $i = ${groups(i).size}")
    }
    playGame(memoryArray)
  }

  def playGame(memoryArray: ArrayBuffer[Long]) = {
    val newMem = memoryArray.clone()
    newMem(0) = 2
    val care = new Care()
    val vm = PreemptiveMultiTasker(newMem)
    val score = care.takeTurns(vm,315, (-1,-1))
    println(s"day 13 part 2 answer $score")

  }
}