package intcoderuntime

class IntCodeInterpreter(memory: Memory) extends MtProcess {

  var console: Console = _
  var ended = false

  def isEnded(): Boolean = ended

  def readNextInstruction(): ((List[Char], List[Long])=>Int, List[Char], List[Long]) = {
    val paramCounts = Map(1-> 3, 2-> 3, 3 -> 1, 4 -> 1, 5-> 2, 6-> 2, 7 -> 3, 8 -> 3, 9 -> 1, 99 -> 0)
    val instructionSet: Map[Int, (List[Char], List[Long]) => Int] = Map(
      1  -> instructionAdd,
      2  -> instructionMultiply,
      3  -> instructionInput,
      4  -> instructionOutput,
      5  -> instructionJumpIfTrue,
      6  -> instructionJumpIfFalse,
      7  -> instructionLessThan,
      8  -> instructionEquals,
      9  -> instructionAdjustBase,
      99 -> instructionEnd)

    val opCode = memory.readAndAdvance
    val paramCount = paramCounts(opCode.toInt % 100)
    val parameterModes: List[Char] = ((opCode / 100).toString.reverse+("0" * (paramCount))).substring(0, paramCount).toList
    val params = memory.readAndAdvance(paramCount)
//    println(s"debug: $opCode:$paramCount:$params")
    (instructionSet(opCode.toInt % 100), parameterModes, params)
  }

  def nonMemParameterValue(mode: Char, param: Long): Long = mode match {
    case '0' => memory.peek(param.toInt)
    case '1' => param
    case '2' => memory.peek(param.toInt, Mode.RELATIVE)
  }

  def memParameterValue(mode: Char, param: Long): Int = mode match {
    case '0' => param.toInt
    case '1' => param.toInt
    case '2' => memory.adjustAddress(param.toInt, Mode.RELATIVE)
  }

  def doMath(params: List[Long], parameterModes: List[Char], operation: (Long, Long)=>Long): Int = {
    val valueA = nonMemParameterValue(parameterModes.head, params.head)
    val valueB = nonMemParameterValue(parameterModes.tail.head, params.tail.head)
    val address = memParameterValue(parameterModes.last, params.last)
    memory.poke(address, operation(valueA, valueB))
    0 // instruction successful
  }

  def instructionAdd(parameterModes: List[Char], params: List[Long]): Int = {
    doMath(params, parameterModes, _ + _)
  }
  def instructionMultiply(parameterModes: List[Char], params: List[Long]): Int = {
    doMath(params, parameterModes, _ * _)
  }

  def instructionInput(parameterModes: List[Char], params: List[Long]):Int = {
    storeInstructionState(parameterModes, params)
    2
  }
  def resumeInput(parameterModes: List[Char], params: List[Long]):Int = {
    val input = console.readLong()
    val address = memParameterValue(parameterModes.head, params.head)
    memory.poke(address, input)
    0
  }
  def instructionOutput(parameterModes: List[Char], params: List[Long]):Int = {
    val output = nonMemParameterValue(parameterModes.head, params.head)
    console.writeLong(output)
    0
  }

  def instructionJumpIfTrue(parameterModes: List[Char], params: List[Long]):Int = {
    val valueA = nonMemParameterValue(parameterModes.head, params.head)
    val newAddress = nonMemParameterValue(parameterModes.tail.head, params.tail.head).toInt
    if (valueA != 0) memory.jumpToAddress(newAddress)
    0
  }
  def instructionJumpIfFalse(parameterModes: List[Char], params: List[Long]):Int = {
    val valueA = nonMemParameterValue(parameterModes.head, params.head)
    val newAddress = nonMemParameterValue(parameterModes.tail.head, params.tail.head).toInt
    if (valueA == 0) memory.jumpToAddress(newAddress)
    0
  }
  def instructionLessThan(parameterModes: List[Char], params: List[Long]):Int = {
    val valueA = nonMemParameterValue(parameterModes.head, params.head)
    val valueB = nonMemParameterValue(parameterModes.tail.head, params.tail.head)
    val location = memParameterValue(parameterModes.tail.tail.head, params.tail.tail.head)
    memory.poke(location, if (valueA < valueB) 1 else 0)
    0
  }
  def instructionEquals(parameterModes: List[Char], params: List[Long]):Int = {
    val valueA = nonMemParameterValue(parameterModes.head, params.head)
    val valueB = nonMemParameterValue(parameterModes.tail.head, params.tail.head)
    val location = memParameterValue(parameterModes.tail.tail.head, params.tail.tail.head)
    memory.poke(location.toInt, if (valueA ==valueB) 1 else 0)
    0
  }
  def instructionAdjustBase(parameterModes: List[Char], params: List[Long]):Int = {
    val valueA = nonMemParameterValue(parameterModes.head, params.head)
    memory.adjustRelativeBase(valueA.toInt)
    0
  }
  def instructionEnd(parameterModes: List[Char], params: List[Long]):Int = 1

  var state_parameterModes: List[Char] = List()
  var state_params: List[Long] = List()

  def storeInstructionState(parameterModes: List[Char], params: List[Long]) = {
    state_parameterModes = parameterModes
    state_params = params
  }

  def runProgram() = {
    if (memory.programCounter != 0) resumeInput(state_parameterModes, state_params)
    var result = 0
    while (result == 0)
    {
      val instruction = readNextInstruction
      result = instruction._1(instruction._2, instruction._3)
    }
    ended = (result == 1)
  }

  override def run(): Unit = {
    runProgram()
  }

  override def checkCanRun(): Boolean = (!console.inputQueue.isEmpty || memory.programCounter == 0) && !ended

  override def getConsole(): Console = console

  override def getMemory(): Memory = memory
}

