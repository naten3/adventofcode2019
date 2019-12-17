package adventofcode

import adventofcode.Day7.Amplifier
import scala.io.Source

object Day7 {
  def main(args: Array[String]): Unit = {
    val numbers = Source.fromResource("day7.txt").getLines.toList(0).split(',').map(_.toInt)
    println(part1(numbers))
    println(part2(numbers))
  }

  def part1(lines: Array[Int]): Int = {
    maxSequence(lines)
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

  case class SequenceWithOutput(sequence: Array[Int], output: Int)

  def maxSequence(lines: Array[Int]): Int = {
    (0 to 4).permutations.map(sequenceOutput(lines, _)).map(_._2).max
  }

  def sequenceOutput(lines: Array[Int], seq: IndexedSeq[Int]): (IndexedSeq[Int], Int) = {
      val output = seq.foldLeft(None: Option[Int])((previousOutput: Option[Int], phaseSetting) => {
        val inputs = List(phaseSetting, previousOutput.getOrElse(0))
        Some(processArray(lines.clone(), inputs))
      }).get
      (seq, output)
  }

  def processArray(lines: Array[Int], input: List[Int]): Int = {
    var position = 0
    var output = 0
    var mutableInputes = input;
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
        lines(lines(position + 1)) = mutableInputes.head
        mutableInputes = mutableInputes.tail
      } else if (opCode == 4) {
        val op = params(0)
        output = op
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

  def part2(lines: Array[Int]): Int = {
    (5 to 9).permutations.map(seq => {
      val amplifiers = seq.map(new Amplifier(lines.clone(), _)).toArray

      var output: Option[Int] = Some(0)
      var previousOutput: Option[Int] = Some(0)
      var index = 0
      var eOutput = 0
      while(output.isDefined) {
        val amplifier = amplifiers(index % amplifiers.length)
        previousOutput = output
        output = amplifier.acceptInput(previousOutput.get, index < amplifiers.length)
        if (output.isDefined && index % amplifiers.size == amplifiers.size - 1) {
          eOutput = output.get
        }
        index += 1
      }

      eOutput
    }).max
  }

  class Amplifier(lines: Array[Int], phaseSetting: Int) {
    var position = 0

    def acceptInput(input: Int, firstRun: Boolean): Option[Int] = {
      val inputs = if (firstRun) List(phaseSetting, input) else List(input)
      processArray(inputs)
    }

    private def processArray(input: List[Int]): Option[Int] = {
      var output = 0
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
          if (mode == 0) lines(param) else param
        }).toList

        if (opCode == 99) {
          return None
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
          lines(lines(position + 1)) = mutableInputs.head
          mutableInputs = mutableInputs.tail
        } else if (opCode == 4) {
          val op = params(0)
          position += positionMove;
          return Some(op)
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
}

