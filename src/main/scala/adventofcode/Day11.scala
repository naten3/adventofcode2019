package adventofcode

import adventofcode.Day3.Direction.Value

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.io.Source

object Day11 {
  def main(args: Array[String]): Unit = {
    val numbers = Source.fromResource("day11.txt").getLines.toList(0).split(',').map(_.toLong).concat((0 to 1000).map(_ => 0L))
    println(part1(numbers.clone()))
    part2(numbers.clone()).foreach(line => println(line.mkString))
  }

  case class Coordinate(x: Int, y: Int)
  object Direction extends Enumeration {
    type Direction = Value
    val Up,Down,Left,Right = Value
  }
  val BLACK = 0
  val WHITE = 1

  val DIRECTIONS = Array(Direction.Up, Direction.Right, Direction.Down, Direction.Left)

  def part1(lines: Array[Long]): Int = {
    paintRoom(None, lines).size
  }

  case class Extremes(minX: Int, maxX: Int, minY: Int, maxY: Int)
  def part2(lines: Array[Long]): Array[Array[Char]] = {
    val colorMap = paintRoom(Some(1), lines)
    val extremes = colorMap.keys.foldLeft(Extremes(0,0,0,0))((result, coord) =>
    Extremes(
      Math.min(result.minX, coord.x),
      Math.max(result.maxX, coord.x),
      Math.min(result.minY, coord.y),
      Math.max(result.maxY, coord.y)
    ))

    val (arHeight, arWidth) = ((extremes.maxY - extremes.minY), (extremes.maxX - extremes.minX))
    val ar: Array[Array[Char]] = (0 to arHeight).map( _ => (0 to arWidth).map(_ => '\u2591' ).toArray).toArray
    colorMap.foreach(entry => {
      val (coord, color) = entry
      ar(coord.y - extremes.minY)(coord.x - extremes.minX) = if (color == 0) '\u2591'  else '\u2588'
    })
    ar.reverse
  }

  def paintRoom(firstSquareColor: Option[Int], lines: Array[Long]): Map[Coordinate, Int] = {
      var coordinate = Coordinate(0,0)
      var direction = 0
      var colorMap = mutable.Map[Coordinate, Int]()
      if (firstSquareColor.isDefined) {
        colorMap.put(coordinate, firstSquareColor.get)
      }

      val computer = new IntComputer(lines)
      var output: Option[Array[Long]] = Some(Array())

      while(output.isDefined) {
        val currentColor = colorMap.getOrElse(coordinate, BLACK)
        output = computer.acceptInput(currentColor)
        if (output.isDefined) {
          val Array(color, turnDirection) = output.get
          colorMap.put(coordinate, color.toInt)
          direction = if(turnDirection == 0) (direction + DIRECTIONS.length - 1) % DIRECTIONS.length else (direction + 1) % DIRECTIONS.length
          coordinate = DIRECTIONS(direction) match {
            case Direction.Up => Coordinate(coordinate.x, coordinate.y + 1)
            case Direction.Left => Coordinate(coordinate.x - 1, coordinate.y)
            case Direction.Down => Coordinate(coordinate.x, coordinate.y - 1)
            case Direction.Right => Coordinate(coordinate.x + 1, coordinate.y)
          }
        }
      }
      colorMap.toMap
  }

  class IntComputer(lines: Array[Long]) {
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

    var position = 0
    var relativeBase = 0

    def acceptInput(input: Long): Option[Array[Long]] = {
      val inputs = List(input)
      processArray(inputs)
    }

    def runNoInput(): Option[Array[Long]] = {
      val inputs = List()
      processArray(inputs)
    }

    private def processArray(input: List[Long]): Option[Array[Long]] = {

      var outputs = List[Long]();
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

          val List(val1, val2, _) = params
          val operationResult = operation(val1, val2)
          val writePosition = if (parameterModes(2) == 2) relativeBase + lines(position + 3).toInt else lines(position + 3).toInt
          lines(writePosition) = operationResult
        } else if (opCode == 3) {
          if (mutableInputs.isEmpty) {
            return Some(outputs.reverse.toArray)
          } else {
            val writePosition = if (parameterModes.head == 2) relativeBase + lines(position + 1).toInt else lines(position + 1).toInt
            lines(writePosition) = mutableInputs.head
            mutableInputs = mutableInputs.tail
          }
        } else if (opCode == 4) {
          val op = params(0)
          outputs = op :: outputs
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

