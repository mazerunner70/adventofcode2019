package day04

import org.scalatest.FlatSpec

class PasswordCalculatorTest extends FlatSpec {

  behavior of "PasswordCalculatorTest"

  it should "containsDigitPair" in {
    val passwordCalculator = new PasswordCalculator(245182, 790572)
    assert(passwordCalculator.containsDigitPair(111111))
    assert(passwordCalculator.containsDigitPair(112345))
    assert(passwordCalculator.containsDigitPair(123445))
  }

  it should "containsDigitTriple" in {
    val passwordCalculator = new PasswordCalculator(245182, 790572)
    assert(passwordCalculator.containsDigitTriple(111111))
    assert(passwordCalculator.containsDigitTriple(111345))
    assert(passwordCalculator.containsDigitTriple(123444))
    assert(passwordCalculator.containsDigitTriple(124448))
    assert(passwordCalculator.containsDigitTriple(124458) == false)
  }

  it should "containsDigitPairOnly" in {
    val passwordCalculator = new PasswordCalculator(245182, 790572)
    assert(passwordCalculator.containsDigitPairOnly(111111) == false)
    assert(passwordCalculator.containsDigitPairOnly(112345))
    assert(passwordCalculator.containsDigitPairOnly(123444) == false)
    assert(passwordCalculator.containsDigitPairOnly(123477))
    assert(passwordCalculator.containsDigitPairOnly(124448) == false)
    assert(passwordCalculator.containsDigitPairOnly(124458))
  }

  it should "isAscending" in {
    val passwordCalculator = new PasswordCalculator(245182, 790572)
    assert(passwordCalculator.isAscending(111111))

  }

    it should "isValidPassword" in {
    val passwordCalculator = new PasswordCalculator(245182, 790572)
    assert(passwordCalculator.isValidPassword1(333333))
    assert(passwordCalculator.isValidPassword1(223450) == false)
    assert(passwordCalculator.isValidPassword1(123789) == false)
  }

}
