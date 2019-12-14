package adventofcode

import scala.io.Source

object Day5 {
  def main(args: Array[String]): Unit = {
    val numbers = Source.fromResource("day5.txt").getLines.toList(0).split(',').map(_.toInt)
    println(part1(numbers))
    println(part2(numbers))
  }

  def part1(lines: Array[Int]): String = {
    processArray(lines.clone(), 1)
  }

  def part2(lines: Array[Int]): String = {
    processArrayPart2(lines.clone(), 5)
  }

  val paramCountsByOpCode = Map(
    1 -> 3,
    2 -> 3,
    3 -> 1,
    4 -> 1,
    5 -> 2,
    6 -> 2,
    7 -> 3,
    8 -> 3,
    99 -> 0
  )

  def processArray(lines: Array[Int], input: Int): String = {
    var position = 0
    var output = ""
    while(true) {
      val currentCode = lines(position).toString;
      val leftPaddedCode: String = ((currentCode.length + 1 to 5).map(_ => '0').mkString + currentCode).reverse
      val opCode = leftPaddedCode.substring(0, 2).reverse.toInt;
      val parameterModes = leftPaddedCode.substring(2).map(_.asDigit).toList

      val paramCount = paramCountsByOpCode(opCode)
      val positionMove = paramCount + 1;
      val params = (0 until paramCount).map( i => {
        val mode = parameterModes(i)
        val param = lines(position + 1 + i)
        if (mode == 0) lines(param) else param
      }).toList

      if (opCode == 99) {
        return output
      }

      if (opCode == 1 || opCode == 2) {
        val operation = if (opCode == 1) {
          (a: Int, b: Int) => a + b
        } else if (opCode == 2) {
          (a: Int, b: Int) => a * b
        } else {
          throw new Exception("this shouldn't happen")
        }

        val List(val1, val2, _) = params
        val operationResult = operation(val1, val2)
        lines(lines(position + 3)) = operationResult
      } else if (opCode == 3) {
        lines(lines(position + 1)) = input;
      } else if (opCode == 4) {
        val op = params(0)
        output += op
      } else {
        throw new Exception("invalid op code " + opCode)
      }

      position += positionMove
    }
    throw new Exception("Something went wrong")
  }

  def processArrayPart2(lines: Array[Int], input: Int): String = {
    var position = 0
    var output = ""
    while(true) {
      val currentCode = lines(position).toString;
      val leftPaddedCode: String = ((currentCode.length + 1 to 5).map(_ => '0').mkString + currentCode).reverse
      val opCode = leftPaddedCode.substring(0, 2).reverse.toInt;
      val parameterModes = leftPaddedCode.substring(2).map(_.asDigit).toList
      var hasJumped = false;

      val paramCount = paramCountsByOpCode(opCode)
      val positionMove = paramCount + 1;
      val params = (0 until paramCount).map( i => {
        val mode = parameterModes(i)
        val param = lines(position + 1 + i)
        if (mode == 0) lines(param) else param
      }).toList

      if (opCode == 99) {
        return output
      }

      if (opCode == 1 || opCode == 2) {
        val operation = if (opCode == 1) {
          (a: Int, b: Int) => a + b
        } else if (opCode == 2) {
          (a: Int, b: Int) => a * b
        } else {
          throw new Exception("this shouldn't happen")
        }

        val List(val1, val2, _) = params
        val operationResult = operation(val1, val2)
        lines(lines(position + 3)) = operationResult
      } else if (opCode == 3) {
        lines(lines(position + 1)) = input;
      } else if (opCode == 4) {
        val op = params(0)
        output += op
      } else if (opCode == 5 || opCode == 6) {
        val List(param1, param2) = params
        val checkFunction = if (opCode == 5) (x: Int) => x != 0 else (x: Int) => x == 0
        if (checkFunction(param1)) {
          position = param2
          hasJumped = true
        }
      } else if (opCode == 7 || opCode == 8) {
        val List(param1, param2, _) = params
        val checkFunction = if (opCode == 7) (a: Int, b: Int) => a < b else  (a: Int, b: Int) => a == b
        lines(lines(position + 3)) = if (checkFunction(param1, param2)) 1 else 0
      } else {
        throw new Exception("invalid op code " + opCode)
      }
      if (!hasJumped) {
        position += positionMove
      }
    }
    throw new Exception("Something went wrong")
  }
}

