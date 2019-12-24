package day11

import common.SourceAware
import intcoderuntime.PreemptiveMultiTasker

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Robot(vm: PreemptiveMultiTasker){

  @tailrec
  final def doInstructions(position : (Int, Int), panels: Map[(Int, Int), Int], direction: Int): Map[(Int, Int), Int] = {
    val offsets = Map(0->(0, -1), 1->(1, 0), 2-> (0, 1), 3->(-1, 0))
    vm.getConsole().addToInput(panels.get(position).getOrElse(0).toLong)
    vm.run()
    vm.isEnded() match {
      case false => {
        val queue = vm.getConsole().outputQueue.get
        val colour = queue.dequeue().toInt
        val newdirection = (direction + queue.dequeue().toInt *2 -1 + 4) % 4
        val offset = offsets(newdirection)
        println(s"position: $position")
        doInstructions((position._1+offset._1, position._2+offset._2), panels + (position->colour), newdirection)
      }
      case true => {
        panels
      }
    }
  }

  def run(): Map[(Int, Int), Int] = {
    doInstructions((0,0), Map(), 0)
  }

  def convertToGrid(points: Map[(Int, Int), Int]): List[String] = {
    val maxX = points.map { case (k, v) => k._1 }.max
    val maxY = points.map { case (k, v) => k._2 }.max
    val grid = List.fill(maxY+1)(ArrayBuffer.fill(maxX+1)(' '))
    for ((k, v) <- points) {
      if (v == 1) grid(k._2)(k._1) = 'X'
    }
    grid.map(_.mkString)
  }

}

object Robot {
  def main(args: Array[String]): Unit = {

    val pageContent = SourceAware.loadFromFile("day11/input.txt")
    val memoryArray = pageContent.split(',').map(_.toLong).to[ArrayBuffer]
    val robot = new Robot(PreemptiveMultiTasker(memoryArray))
    val covered = robot.run()
    println(s"day 11 part 1 answer= ${covered.size}")

    val robot2 = new Robot(PreemptiveMultiTasker(memoryArray))
    val covered2 = robot2.doInstructions((0,0), Map((0,0)->1), 0)
    val grid = robot2.convertToGrid(covered2)
    grid.foreach(println(_))
  }

}