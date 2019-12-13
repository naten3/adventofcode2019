package adventofcode

import adventofcode.Day3.Direction.Direction
import adventofcode.Day3.Orientation.Orientation
import adventofcode.Day4.rangeStartAr

import scala.io.Source
import scala.jdk.IntAccumulator
import scala.math.{abs, max, min}

object Day4 {
  val rangeStart = 387638
  val rangeEnd = 919123
  val rangeStartAr = rangeStart.toString.map(_.asDigit).toArray
  val rangeEndAr = rangeEnd.toString.map(_.asDigit).toArray
  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }

  def part1(): Int = {
    combinations()
  }

  def part2(): Int = {
    combinationsNoGroupsOf3()
  }

  /**
   * @param atLowerBound: Do all the previous digits match the lower bound
   * @param atUpperBound: Do all the previous digits match the upper bound
   * @return
   */
    def combinations(): Int = {
      def combinations(chosenDigits: List[Int], hasTwoSequential: Boolean, atLowerBound: Boolean, atUpperBound: Boolean): Int = {
        if (chosenDigits.length == 6) {
          return if (hasTwoSequential) 1 else 0
        }
        getRange(chosenDigits, atLowerBound, atUpperBound).map(digit => {
          val newChosenDigits = digit :: chosenDigits
          val newAtLowerBound = (atLowerBound || chosenDigits.isEmpty) && digit == rangeStartAr(chosenDigits.length)
          val newAtUpperBound = (atUpperBound || chosenDigits.isEmpty) &&  digit == rangeEndAr(chosenDigits.length)
          val newHasTwoSequential = hasTwoSequential || chosenDigits.nonEmpty && digit == chosenDigits.head
          combinations(newChosenDigits, newHasTwoSequential, newAtLowerBound, newAtUpperBound)
        }).sum
      }
      combinations(List(), false, true, false)
  }

  def getRange(chosenDigits: List[Int], atLowerBound: Boolean, atUpperBound: Boolean): Iterable[Int] = {
    if(chosenDigits.isEmpty) {
      return (rangeStartAr(0) to rangeEndAr(0))
    }
    val lowerBound = if (atLowerBound) Some(rangeStartAr(chosenDigits.length)) else None
    val previousDigit = Some(chosenDigits.head)
    val rangeStart = List(lowerBound, previousDigit).flatten.max
    val rangeEnd = if (atUpperBound) rangeEndAr(chosenDigits.length) else 9
    rangeStart to rangeEnd
  }

  def combinationsNoGroupsOf3(): Int = {
    def combinationsNoGroupsOf3(chosenDigits: List[Int], hasTwoSequential: Boolean, atLowerBound: Boolean, atUpperBound: Boolean): Int = {
      if (chosenDigits.length == 6) {
        return if (hasTwoSequential && hasTwoNonGrouped(chosenDigits)) 1 else 0
      }
      getRange(chosenDigits, atLowerBound, atUpperBound)
        .map(digit => {
        val newChosenDigits = digit :: chosenDigits
        val newAtLowerBound = (atLowerBound || chosenDigits.isEmpty) && digit == rangeStartAr(chosenDigits.length)
        val newAtUpperBound = (atUpperBound || chosenDigits.isEmpty) &&  digit == rangeEndAr(chosenDigits.length)
        val newHasTwoSequential = hasTwoSequential || chosenDigits.nonEmpty && digit == chosenDigits.head
        combinationsNoGroupsOf3(newChosenDigits, newHasTwoSequential, newAtLowerBound, newAtUpperBound)
        combinationsNoGroupsOf3(newChosenDigits, newHasTwoSequential, newAtLowerBound, newAtUpperBound)
      }).sum
    }
    combinationsNoGroupsOf3(List(), false, true, false)
  }

  def hasTwoNonGrouped(digits: Iterable[Int]): Boolean = {
    var currentDigit: Int = -1
    var groupSize: Int = 0
    digits.foreach(d => {
      if (d != currentDigit) {
        if (groupSize == 2) {
          return true
        }
        groupSize = 0
        currentDigit = d
      }
      groupSize+=1
    })

    groupSize == 2
  }
}


