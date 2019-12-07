package day05

class Terminal {

  var lastOutput = 0

  def readInt(): Int = 1

  def writeInt(value: Int) = {
    println(s"Int written = $value")
    lastOutput = value
  }

}
