package day08

import day07.AmplifierV2.loadFromFile

import scala.io.Source

class SpaceImageUtils {

  def getLayers(imageData:String, pixelsWide: Int, pixelsTall: Int): List[String] = {
    imageData.grouped(pixelsTall * pixelsWide).toList
  }

  def getDigitFrequencies(layers: List[String], digit: Char): List[Int] = layers match {
    case layer :: tail => layer.count(_ == digit) :: getDigitFrequencies(tail, digit)
    case Nil           => Nil
  }

  def getLayerWithLeastZero(layers: List[String]): String = {
    val digitFrequencies = getDigitFrequencies(layers, '0')
    val targetLayerCount = digitFrequencies.min
    val zipped = layers zip digitFrequencies
    zipped.find(_._2 == targetLayerCount) match {
      case None => throw new Exception
      case Some(layer) => layer._1
    }
  }

  def calculateFinalImage(layers: List[String]): String = {
    val transposed = layers.transpose
    val image = transposed.map(_.find(_ != '2').getOrElse('2'))
    image.mkString
  }

  def visualise(image: String, width: Int, height: Int): Unit = {
    println(image.grouped(width).mkString("\n").replaceAllLiterally("0", " "))
  }

}

object SpaceImageUtils {

  def loadFromFile(filename:String): String = {
    Source.fromResource(filename).mkString
  }


  def main(args: Array[String]): Unit = {
    val pageContent = loadFromFile("day08/input.txt")

    val spaceImageUtils = new SpaceImageUtils
    val layers = spaceImageUtils.getLayers(pageContent, 25, 6)
    val layer = spaceImageUtils.getLayerWithLeastZero(layers)
    val answer = layer.count(_ == '1') * layer.count(_ == '2')
    println(s"Day 08 part 1 answer: $answer")
    println(s"Day 08 part 2 answer: ${spaceImageUtils.calculateFinalImage(layers)}")
    println(spaceImageUtils.visualise(spaceImageUtils.calculateFinalImage(layers), 25, 6))
  }
}