package day09


import scala.collection.mutable

trait Terminal {
  def readLong(): Long
  def writeLong(value: Long): Unit
}



class Console extends Terminal{

  val inputList: mutable.Queue[Long] = mutable.Queue()
  var writesTo : Option[Console] = None
  var lastOutput: Option[Long] = None

  override def readLong(): Long = inputList.dequeue()

  override def writeLong(value: Long): Unit = {
    writesTo.foreach{_.inputList.enqueue(value)}
    println(s" Console output: $value")
    lastOutput = Some(value)
  }
}
