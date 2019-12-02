package day1

import io.Source._
import scala.io.Source

object FuelCounterUpper {

  def total(list: List[Int]) : Int = {
    list.map(calculateFuel).sum
  }

  def calculateFuel(mass: Int) : Int =
    mass/3-2

  def loadFromFile(filename:String): String = {
    Source.fromResource(filename).mkString
  }
  def main(args: Array[String]): Unit = {
    val pageContent = loadFromFile("day1/input.txt")
    val list = pageContent.split('\n').map(_.toInt).toList
    println(total(list))
  }

}
