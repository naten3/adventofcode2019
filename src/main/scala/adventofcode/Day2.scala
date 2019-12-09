package adventofcode

import scala.io.Source

object Day2 {
  def main(args: Array[String]): Unit = {
    val numbers = Source.fromResource("day2.txt").getLines.toList(0).split(',').map(_.toInt)
    println(part1(numbers))
    println(part2(numbers))
  }

  def part1(lines: Array[Int]): Int = {
    val copy = lines.clone()
    copy(1) = 12
    copy(2) = 2
    processArray(copy)
  }

  def processArray(lines: Array[Int]): Int = {
    var position = 0;
    while(true) {
      val opCode = lines(position)
      if (opCode == 99) {
        return lines(0)
      }
      val first = lines(lines(position + 1))
      val second = lines(lines(position + 2))
      val operation = if (opCode == 1) {
        (a: Int,b: Int) => a + b
      } else if (opCode == 2) {
        (a: Int, b: Int) => a * b
      } else throw new Exception("Unknown opcode")
      lines(lines(position + 3)) = operation(first, second)
      position += 4
    }
    throw new Exception("Error") //we shouldn't get here ever
  }

  def part2(lines: Array[Int]): Int = {
    val (noun, verb) = (0 to 99).combinations(2)
      .map( seq => ((seq(0), seq(1))))
      .find( ab => {
        val listCopy = lines.clone()
        listCopy(1) = ab._1
        listCopy(2) = ab._2
        try {
          processArray(listCopy) == 19690720
        } catch {
          case _ => false
        }
      }).get
    100 * noun + verb
  }
}

