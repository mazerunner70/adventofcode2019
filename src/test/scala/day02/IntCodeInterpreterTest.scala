package day02

import org.scalatest.FlatSpec

class IntCodeInterpreterTest extends FlatSpec {

  behavior of "IntCodeInterpreterTest"

  it should "readAdvance" in {

  }

  it should "isProgramEnded" in {

  }

  it should "runProgram1" in {
    val memory = Array(1,0,0,0,99)
    val endMemory = Array(2,0,0,0,99)
    val intCodeInterpreter = new IntCodeInterpreter(memory)
    intCodeInterpreter.runProgram()
    assert(endMemory.deep == memory.deep)
  }

  it should "runProgram2" in {
    val memory = Array(2,3,0,3,99)
    val endMemory = Array(2,3,0,6,99)
    val intCodeInterpreter = new IntCodeInterpreter(memory)
    intCodeInterpreter.runProgram()
    assert(endMemory.deep == memory.deep)
  }

  it should "runProgram3" in {
    val memory = Array(2,4,4,5,99,0)
    val endMemory = Array(2,4,4,5,99,9801)
    val intCodeInterpreter = new IntCodeInterpreter(memory)
    intCodeInterpreter.runProgram()
    assert(endMemory.deep == memory.deep)
  }

  it should "runProgram4" in {
    val memory = Array(1,1,1,4,99,5,6,0,99)
    val endMemory = Array(30,1,1,4,2,5,6,0,99)
    val intCodeInterpreter = new IntCodeInterpreter(memory)
    intCodeInterpreter.runProgram()
    assert(endMemory.deep == memory.deep)
  }

  it should "programCounter" in {

  }

  it should "programCounter_$eq" in {

  }

  it should "executeCommand" in {

  }

}
