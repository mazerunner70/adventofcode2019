package day07

import org.scalatest.FlatSpec

class AmplifierTest extends FlatSpec {

  behavior of "AmplifierTest"

  it should "runAmplifierList" in {
    val memory = Array(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0)
    val amplifier = new Amplifier()
    val outputSignal = amplifier.runAmplifierList(memory, List(4,3,2,1,0), 0)
    assert (outputSignal == 43210)
  }

  it should "runAmplifierList2" in {
    val memory = Array(3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0)
    val amplifier = new Amplifier()
    val outputSignal = amplifier.runAmplifierList(memory, List(0,1,2,3,4), 0)
    assert (outputSignal == 54321)
  }

  it should "runAmplifierList3" in {
    val memory = Array(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0)
    val amplifier = new Amplifier()
    val outputSignal = amplifier.runAmplifierList(memory, List(1,0,4,3,2), 0)
    assert (outputSignal == 65210)
  }

  it should "demoCombinations" in {
    val list = List(1,0,4,3,2)
    println(list.permutations.toList)
  }

  it should "runAmplifierList4" in {
    val memory = Array(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5)
    val amplifier = new Amplifier()
    val outputSignal = amplifier.runAmplifierList(memory, List(9,8,7,6,5), 0)
    assert (outputSignal == 139629729)
  }



}
