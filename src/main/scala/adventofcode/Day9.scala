package adventofcode

import adventofcode.Day7.Amplifier
import scala.io.Source

object Day9 {
  def main(args: Array[String]): Unit = {
    val numbers = Source.fromResource("day9.txt").getLines.toList(0).split(',').map(_.toLong).concat((0 to 1000).map(_ => 0L))
    println(part1(numbers))
    println(part2(numbers))
  }

  def part1(lines: Array[Long]): Long = {
    val amplifier = new Amplifier(lines.clone())
    var output: Option[Long] = Some(0L)
    var lastOutput: Long = 0L
    while(output.isDefined) {
      output = amplifier.acceptInput(1L)
      if (output.isDefined) {
        lastOutput = output.get
      }
    }
    lastOutput
  }

  def part2(lines: Array[Long]): Long = {
    val amplifier = new Amplifier(lines.clone())
    var output: Option[Long] = Some(0L)
    var lastOutput: Long = 0L
    while(output.isDefined) {
      output = amplifier.acceptInput(2L)
      if (output.isDefined) {
        lastOutput = output.get
      }
    }
    lastOutput
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
    9 -> 1,
    99 -> 0
  )

  class Amplifier(lines: Array[Long]) {
    var position = 0
    var relativeBase = 0

    def acceptInput(input: Long): Option[Long] = {
      val inputs = List(input)
      processArray(inputs)
    }

    def runNoInput(): Option[Long] = {
      val inputs = List()
      processArray(inputs)
    }

    private def processArray(input: List[Long]): Option[Long] = {
      var mutableInputs = input;
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
          mode match {
            case 0 => lines(param.toInt)
            case 1 => param
            case 2 => lines(relativeBase + param.toInt)
          }
        }).toList

        if (opCode == 99) {
          return None
        }

        if (opCode == 1 || opCode == 2) {
          val operation = if (opCode == 1) {
            (a: Long, b: Long) => a + b
          } else if (opCode == 2) {
            (a: Long, b: Long) => a * b
          } else {
            throw new Exception("this shouldn't happen")
          }

          val List(val1, val2, param3) = params
          val operationResult = operation(val1, val2)
          val writePosition = if (parameterModes(2) == 2) relativeBase + lines(position + 3).toInt else lines(position + 3).toInt
          lines(writePosition) = operationResult
        } else if (opCode == 3) {
          val writePosition = if (parameterModes.head == 2) relativeBase + lines(position + 1).toInt else lines(position + 1).toInt
          lines(writePosition) = mutableInputs.head
          mutableInputs = mutableInputs.tail
        } else if (opCode == 4) {
          val op = params(0)
          position += positionMove
          return Some(op)
        } else if (opCode == 5 || opCode == 6) {
          val List(param1: Long, param2: Long) = params
          val checkFunction = if (opCode == 5) (x: Long) => x != 0 else (x: Long) => x == 0
          if (checkFunction(param1)) {
            position = param2.toInt
            hasJumped = true
          }
        } else if (opCode == 7 || opCode == 8) {
          val List(param1, param2, param3) = params
          val checkFunction = if (opCode == 7) (a: Long, b: Long) => a < b else  (a: Long, b: Long) => a == b
          val writePosition = if (parameterModes(2) == 2) relativeBase + lines(position + 3).toInt else lines(position + 3).toInt
          lines(writePosition) = if (checkFunction(param1, param2)) 1 else 0
        } else if (opCode == 9) {
          relativeBase += params(0).toInt
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
}

