package intcoderuntime

import org.scalatest.FlatSpec

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class PreemptiveMultiTaskerTest extends FlatSpec {

  def loadFromFile(filename:String): String = {
    Source.fromResource(filename).mkString
  }


  it should "day2-1" in {
    val mem = ArrayBuffer(1L,0,0,0,99)
    val vm = PreemptiveMultiTasker(mem)
    vm.run()
    assert(vm.processes.head.getMemory().memory == ArrayBuffer(2,0,0,0,99))
  }

  it should "day2-2" in {
    val mem = ArrayBuffer(1L,1,1,4,99,5,6,0,99)
    val vm = PreemptiveMultiTasker(mem)
    vm.run()
    assert(vm.processes.head.getMemory().memory == ArrayBuffer(30,1,1,4,2,5,6,0,99))
  }

  it should "day2-3" in {
    val pageContent = loadFromFile("intcoderuntime/day02/test-input-1.txt")
    val memoryArray = pageContent.split(',').map(_.toLong).to[ArrayBuffer]
    memoryArray(1) = 12L
    memoryArray(2) = 2L
    val vm = PreemptiveMultiTasker(memoryArray)
    vm.run()
    assert(vm.processes.head.getMemory().memory(0) == 6730673)
  }

  it should "day2-4" in {
    val pageContent = loadFromFile("intcoderuntime/day02/test-input-1.txt")
    val memoryArray = pageContent.split(',').map(_.toLong).to[ArrayBuffer]
    def loop(i: Int): Int = {
      memoryArray(1) = i / 100
      memoryArray(2) = i % 100
      val vm = PreemptiveMultiTasker(memoryArray)
      vm.run()
      vm.processes.head.getMemory().memory(0) match {
        case 19690720 => i
        case _ => {
          if (i==9999) 0 else loop(i+1)
        }
      }
    }

    assert(loop(0) == 3749)
  }




}
