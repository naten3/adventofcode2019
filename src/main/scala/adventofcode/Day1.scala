package adventofcode

import scala.annotation.tailrec
import scala.io.Source
import scala.jdk.IntAccumulator

object Day1 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day1.txt").getLines.toList

    println(part1(lines))
    println(part2(lines))
  }

  def part1(lines: List[String]): Int = {
    lines.map(_.toInt/3 - 2).sum
  }

  def part2(lines: List[String]): Int = {
    lines.map(fuel => moduleFuel(fuel.toInt)).sum
  }

  def moduleFuel(initialFuel: Int): Int = {
    @tailrec
    def moduleFuel(initialFuel: Int, acc: IntAccumulator): Unit = {
      val mf = initialFuel / 3 - 2
      if(mf > 0) {
        acc.addOne(mf);
        moduleFuel(mf, acc)
      }
    }

    val acc = IntAccumulator(0);
    moduleFuel(initialFuel, acc)
    acc.sum
  }
}

