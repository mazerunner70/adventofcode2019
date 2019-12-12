package day09

import org.scalatest.FlatSpec

import scala.collection.mutable.ArrayBuffer

class SensorBoostTest extends FlatSpec {

  behavior of "SensorBoostTest"

  it should "runOneProcess" in {
    val memory = ArrayBuffer[Long](109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99)
    val sensorBoost = new SensorBoost
    val (preemptiveMultiTasker, console) = sensorBoost.runOneProcess(memory)
    preemptiveMultiTasker.run()
    assert(console.lastOutput == Some(99))

  }
  it should "runOneProcess2" in {
    val memory = ArrayBuffer[Long](1102,34915192,34915192,7,4,7,99,0)
    val sensorBoost = new SensorBoost
    val (preemptiveMultiTasker, console) = sensorBoost.runOneProcess(memory)
    preemptiveMultiTasker.run()
    assert(console.lastOutput == Some(1219070632396864L))

  }
  it should "runOneProcess3" in {
    val memory = ArrayBuffer[Long](104,1125899906842624L,99)
    val sensorBoost = new SensorBoost
    val (preemptiveMultiTasker, console) = sensorBoost.runOneProcess(memory)
    preemptiveMultiTasker.run()
    assert(console.lastOutput == Some(1125899906842624L))

  }

  it should "runOneProcess4" in {
    val memory = ArrayBuffer[Long](109, -1, 204, 1, 99)
    val sensorBoost = new SensorBoost
    val (preemptiveMultiTasker, console) = sensorBoost.runOneProcess(memory)
    preemptiveMultiTasker.run()
    assert(console.lastOutput == Some(109))

  }
  it should "runOneProcess5" in {
    val memory = ArrayBuffer[Long](109, 1, 9, 2, 204, -6, 99)
    val sensorBoost = new SensorBoost
    val (preemptiveMultiTasker, console) = sensorBoost.runOneProcess(memory)
    preemptiveMultiTasker.run()
    assert(console.lastOutput == Some(204))

  }

  it should "runOneProcess6" in {
    val memory = ArrayBuffer[Long](109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    val sensorBoost = new SensorBoost
    val (preemptiveMultiTasker, console) = sensorBoost.runOneProcess(memory)
    preemptiveMultiTasker.run()
    assert(console.lastOutput == Some(99))

  }
  it should "runOneProcess7" in {
    val memory = ArrayBuffer[Long](3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9)
    val sensorBoost = new SensorBoost
    val (preemptiveMultiTasker, console) = sensorBoost.runOneProcess(memory)
    console.inputList.enqueue(155)
    preemptiveMultiTasker.run()
    assert(console.lastOutput == Some(1))

  }
  it should "runOneProcess8" in {
    val memory = ArrayBuffer[Long](3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)
    val sensorBoost = new SensorBoost
    val (preemptiveMultiTasker, console) = sensorBoost.runOneProcess(memory)
    console.inputList.enqueue(8)
    preemptiveMultiTasker.run()
    assert(console.lastOutput == Some(1000))

  }

}
