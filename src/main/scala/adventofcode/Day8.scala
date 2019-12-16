package adventofcode

import scala.collection.mutable
import scala.io.Source

object Day8 {
  val WIDTH = 25;
  val HEIGHT = 6;
  def main(args: Array[String]): Unit = {
    val chars = Source.fromResource("day8.txt").getLines().mkString.toCharArray
    println(part1(chars))
    part2(chars).foreach(ar => println(ar.mkString))
  }

  def part1(chars: Array[Char]): Int = {
    val maxLayer = chars.grouped(WIDTH * HEIGHT).map(_.groupBy(identity)).minBy(m => m.getOrElse('0', Array()).length)
    maxLayer.getOrElse('1', Array()).length * maxLayer.getOrElse('2', Array()).length
  }

  def part2(chars: Array[Char]): Array[Array[Char]] = {
    chars.grouped(WIDTH * HEIGHT).map(_.toArray).toArray.transpose
      .map(_.filter(char => char == '0' || char == '1').headOption.getOrElse('2'))
      .map(_ match {
        case '0' => '\u2588'
        case '1' => '\u2591'
      }).grouped(WIDTH).toArray
  }

}

