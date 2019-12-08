package day05

import org.scalatest.FlatSpec

class IntCodeInterpreterV2Test extends FlatSpec {

  behavior of "IntCodeInterpreterV2Test"

  it should "runProgram1" in {
    val memory = Array(3,3,1108,-1,8,3,4,3,99)
    val memClone = memory.clone()
    val terminal = new TerminalImpl(8)
    val intCodeInterpreterV2 = new IntCodeInterpreterV2(memClone, terminal)
    intCodeInterpreterV2.runProgram()
    assert(terminal.lastOutput == 1)

  }

  it should "runProgram2" in {
    val memory = Array(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9)
    val memClone = memory.clone()
    val terminal = new TerminalImpl(8)
    val intCodeInterpreterV2 = new IntCodeInterpreterV2(memClone, terminal)
    intCodeInterpreterV2.runProgram()
    assert(terminal.lastOutput == 1)

  }
  it should "runProgram3" in {
    val memory = Array(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9)
    val memClone = memory.clone()
    val terminal = new TerminalImpl(0)
    val intCodeInterpreterV2 = new IntCodeInterpreterV2(memClone, terminal)
    intCodeInterpreterV2.runProgram()
    assert(terminal.lastOutput == 0)

  }

}
