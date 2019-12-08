package day05

trait Terminal {
  def readInt(): Int
  def writeInt(value: Int): Unit
}

class TerminalImpl(defaultInput: Int) extends Terminal {

  var lastOutput = 0

  def readInt(): Int = defaultInput

  def writeInt(value: Int) = {
    println(s"Int written = $value")
    lastOutput = value
  }

}
