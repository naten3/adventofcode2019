package adventofcode

import scala.io.Source

object Day16 {
  val basePattern = List(0,1,0,-1);

  def main(args: Array[String]): Unit = {
    val digits = Source.fromResource("day16.txt").getLines().mkString.toCharArray.map(_.asDigit);

    println(part1(digits))
    println(part2(digits))
  }

  def part1(digits: Array[Int]): String = {
    val patterns = getPatterns(digits.length);
    (1 to 100).foldLeft(digits)((newDigits, _) => getOutputSequence(patterns, newDigits)).take(8).mkString
  }

  def part2(digits: Array[Int]): String = {
    val skipNumber = digits.take(7).mkString.toInt
    val newDigits = (1 to 1000).flatMap(_ => digits).toArray
    val patterns = getPatterns(newDigits.length)
    (1 to 100).foldLeft(newDigits)((nd, _) => getOutputSequence(patterns, nd)).slice(skipNumber,skipNumber + 8).mkString
  }

  def getOutputSequence(patterns: Array[Array[Int]], digits: Array[Int]): Array[Int] = {
    patterns.map(pattern => {
      val sum = pattern.zip(digits).map(_ match {
        case (multiple, digit) => multiple * digit
      }).sum
      Math.abs(sum) % 10
    })
  }

  def getPatterns(length: Int): Array[Array[Int]] = {
    (1 to length).map( n => {
      val pattern = basePattern.flatMap( patternDigit => (1 to n).map(_ => patternDigit))
      (1 to length).map(m => pattern(m % pattern.length)).toArray
    }).toArray
  }

}

