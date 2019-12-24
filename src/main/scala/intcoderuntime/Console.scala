package intcoderuntime

import scala.collection.mutable

trait Terminal {
  def readLong(): Long
  def writeLong(value: Long): Unit
}



class Console extends Terminal{

  val inputQueue: mutable.Queue[Long] = mutable.Queue()
  var outputQueue : Option[mutable.Queue[Long]] = Some(mutable.Queue())

  override def readLong(): Long = inputQueue.dequeue()

  override def writeLong(value: Long): Unit = {
    outputQueue.foreach{_.enqueue(value)}
    println(s" Console output: $value")
  }

  def addToInput(value: Long) = inputQueue.enqueue(value)

  def attachOutput(queue: mutable.Queue[Long]): Unit = {
    outputQueue = Some(queue)
  }
}
