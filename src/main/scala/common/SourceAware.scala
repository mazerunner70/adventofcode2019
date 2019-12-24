package common

import scala.io.Source

object SourceAware {

  def loadFromFile(filename:String): String = {
    Source.fromResource(filename).mkString
  }

}
