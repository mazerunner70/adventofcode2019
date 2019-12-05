package day04

import scala.annotation.tailrec

class PasswordCalculator(rangeLower: Int, rangeUpper: Int) {

  def containsDigitPair(candidate: Int): Boolean = {
    @tailrec
    def checkDuplicate(chars: List[Char]): Boolean = {
      if (chars.head == chars.tail.head) return true
      val t = chars.tail
      t match {
        case f :: Nil => false
        case _        => checkDuplicate(t)
      }
    }
    checkDuplicate(candidate.toString().toList)
  }

  def containsDigitPairOnly(candidate: Int): Boolean = {
    @tailrec
    def checkDuplicateOnly(chars: List[Char], ignore: Char): Boolean = {
      val h = chars.head
      if (h != ignore && h == chars.tail.head && h != chars.tail.tail.head) return true
      val t = chars.tail
      t match {
        case f :: s :: Nil => f == s && f != h
        case _        => checkDuplicateOnly(t, h)
      }
    }
    checkDuplicateOnly(candidate.toString().toList, 0)
  }

  def containsDigitTriple(candidate: Int): Boolean = {
    @tailrec
    def checkTriplicate(chars: List[Char]): Boolean = {
      val h = chars.head
      val h1 = chars.tail.head
      val h2 = chars.tail.tail.head
      val t = chars.tail
      if (h == h1 && h1 == h2) return true
      t match {
        case f :: s :: Nil => false
        case _             => checkTriplicate(t)
      }
    }
    checkTriplicate(candidate.toString().toList)
  }




  def isAscending(candidate: Int): Boolean = {
    @tailrec
    def checkAscending(chars: List[Char]): Boolean = {
      val h = chars.head
      val h1 = chars.tail.head
      if (h>h1) return false
      chars match {
        case f :: s :: Nil => true
        case _   => checkAscending(chars.tail)
      }
    }
    checkAscending(candidate.toString().toList)

  }

  def isValidPassword1(candidate: Int): Boolean = {
    ((rangeLower to rangeUpper contains(candidate))
      && (containsDigitPair(candidate))
      && (isAscending(candidate)))
  }

  def isValidPassword2(candidate: Int): Boolean = {
    ((rangeLower to rangeUpper contains(candidate))
      && (containsDigitPairOnly(candidate))
      && (isAscending(candidate)))
  }


  def validPasswordCounter(validityTest: (Int) => Boolean): Int = {
    var ctr = 0
    for (number <- rangeLower to rangeUpper) {
      if (validityTest(number)) ctr += 1
    }
    ctr
  }

}

object PasswordCalculator {
  def main(args: Array[String]): Unit = {
    val passwordCalculator = new PasswordCalculator(245182, 790572)
    val count1 = passwordCalculator.validPasswordCounter(passwordCalculator.isValidPassword1)
    println(s"Day 04 part 1 answer = $count1")
    val count2 = passwordCalculator.validPasswordCounter(passwordCalculator.isValidPassword2)
    println(s"Day 04 part 2 answer = $count2")
  }
}
