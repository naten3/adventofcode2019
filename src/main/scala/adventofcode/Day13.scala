package adventofcode

import adventofcode.Day11.Direction.Value

import scala.collection.mutable
import scala.io.Source
import scala.util.Random

object Day13 {
  def main(args: Array[String]): Unit = {
    val numbers = Source.fromResource("day13.txt").getLines.toList(0).split(',').map(_.toLong).concat((0 to 1000).map(_ => 0L))
    println(part1(numbers.clone()))
    println(part2(numbers.clone()))
  }

  object TileType extends Enumeration {
    val Empty = Value(0)
    val Wall = Value(1)
    val Block = Value(2)
    val HPaddle = Value(3)
    val Ball = Value(4)
  }

  val tileTypes = Map(
    (0 -> TileType.Empty),
    (1 -> TileType.Wall),
    (2 -> TileType.Block),
    (3 -> TileType.HPaddle),
    (4 -> TileType.Ball)
  )

  def part1(lines: Array[Long]): Int = {
    val computer = new IntComputer(lines)
    val board = setupGame(computer)
    board.map(_.filter(_ == TileType.Block.id).length).sum
  }

  def part2(lines: Array[Long]): Long = {
    lines(0) = 2
    val computer = new IntComputer(lines)
    runGame(computer)
  }

  def setupGame(computer: IntComputer): Array[Array[Int]] = {
    val board: Array[Array[Int]] = (1 to 55).map(_ => (1 to 55).map(_ => 0).toArray).toArray

    val (output, _) = computer.acceptInput(List())

    output.grouped(3).foreach(group => {
      val Array(x, y, tileType) = group
      board(y.toInt)(x.toInt) = tileType.toInt
    })
    board
  }

  def runGame(computer: IntComputer): Long = {
    val random = new Random()
    val acceptedInputes = Array(0, 1, -1)
    val board: Array[Array[Int]] = (1 to 25).map(_ => (1 to 55).map(_ => 0).toArray).toArray
    var (output, terminated) = computer.acceptInput(List())
    var score = 0L
    var input = 0
    do {
      output.grouped(3).foreach(group => {
        val Array(x, y, tileType) = group
        if (x == -1) {
          score = tileType
        } else {
          board(y.toInt)(x.toInt) = tileType.toInt
        }

      })
      printBoard(board, score)
      val result = computer.acceptInput(findMove(board))
      output = result._1
      terminated = result._2
    } while(!terminated)

    output.grouped(3).foreach(group => {
      val Array(x, y, tileType) = group
      if (x == -1) {
        score = tileType
      } else {
        board(y.toInt)(x.toInt) = tileType.toInt
      }

    })

    score
  }

  def findMove(board: Array[Array[Int]]): List[Long] = {
      var (ballX, ballY, paddleX): (Option[Int], Option[Int], Option[Int]) = (None, None, None)
      board.zipWithIndex.foreach( rowWithY => {
        val (row, y) = rowWithY
        row.zipWithIndex.foreach(vWithX => {
          val (v,x) = vWithX
          if ( v == TileType.HPaddle.id) {
            paddleX = Some(x)
          } else if (v == TileType.Ball.id) {
            ballX = Some(x)
            ballY = Some(y)
          }
        })
      })
      if (ballX.get < paddleX.get) {
        List(-1)
      } else if (ballX.get > paddleX.get) {
        List(1)
      } else {
        List(0)
      }
  }

  def printBoard(board: Array[Array[Int]], score: Long) = {
    board.foreach(row => println(row.map( c => {
     val tileType = tileTypes.get(c).get
      tileType match {
        case TileType.Block => '\u2588'
        case TileType.Ball => '\u25cf'
        case TileType.HPaddle => '_'
        case TileType.Empty => ' '
        case TileType.Wall => '|'
      }
    }).mkString))
    println( score)
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

    def acceptInput(inputs: List[Long]): (Array[Long], Boolean) = {
      processArray(inputs)
    }

    private def processArray(input: List[Long]): (Array[Long], Boolean) = {
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
          return (outputs.reverse.toArray, true)
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
            return (outputs.reverse.toArray, false)
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
          if (checkFunction(param1) /*|| position == 369*/) {
            position = param2.toInt
            hasJumped = true
          }
        } else if (opCode == 7 || opCode == 8) {
          val List(param1, param2, _) = params
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

