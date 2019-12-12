package day08

import org.scalatest.FlatSpec

class SpaceImageUtilsTest extends FlatSpec {

  behavior of "SpaceImageUtilsTest"

  it should "calculateFinalImage" in {
    val spaceImageUtils = new SpaceImageUtils
    val layers = spaceImageUtils.getLayers("0222112222120000", 2, 2)
    val result = spaceImageUtils.calculateFinalImage(layers)
    assert(result == "0110")
  }

}
