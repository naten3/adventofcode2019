package adventofcode

import adventofcode.Day13.IntComputer

import scala.collection.mutable
import scala.io.Source
import scala.util.Random

object Day15 {
  def main(args: Array[String]): Unit = {
    val numbers = Source.fromResource("day15.txt").getLines.toList(0).split(',').map(_.toLong).concat((0 to 1000).map(_ => 0L))
    println(part1(numbers.clone()))
    println(part2(numbers.clone()))
  }

  object Direction extends Enumeration {
    val North = Value(1)
    val South = Value(2)
    val West = Value(3)
    val East = Value(4)
  }

  val tileTypes = Map(
    (1 -> Direction.North),
    (2 -> Direction.South),
    (3 -> Direction.West),
    (4 -> Direction.East)
  )

  val opposites = Map((Direction.North -> Direction.South),
    (Direction.South -> Direction.North),
    (Direction.East -> Direction.West),
    (Direction.West -> Direction.East))


  def part1(lines: Array[Long]): Int = {
    val computer = new IntComputer(lines)
    traverse(computer)
  }

  def part2(lines: Array[Long]): Int = {
    val computer = new IntComputer(lines)
    val (spaces, oxygen) = mapArea(computer)
    oxygenFillTime(spaces, oxygen)
  }

  case class Coordinate(x: Int, y: Int)

  def traverse(computer: IntComputer): Int = {
    val walls: mutable.Set[Coordinate] = mutable.Set()

    def shortestPath(location: Coordinate, visited: Set[Coordinate], path: List[Coordinate]): Option[Int] = {
      val neighbors = List(
        (Direction.North, Coordinate(location.x, location.y + 1)),
        (Direction.South, Coordinate(location.x, location.y - 1)),
        (Direction.East, Coordinate(location.x + 1, location.y)),
        (Direction.West, Coordinate(location.x - 1, location.y)))

      val newVisited = visited + location
      val newPath = location :: path

      neighbors
        .filter(neighbor => !visited.contains(neighbor._2) && !walls.contains(neighbor._2))
        .flatMap(neighbor => {
          val (direction, newCoord) = neighbor

          // move in that direction
          val output = computer.acceptInput(List(direction.id))._1(0)

          val result: Option[Int] = if (output == 2) {
            // we found oxygen, revert move and report distance
            computer.acceptInput(List(opposites.get(direction).get.id))
            Some(path.length + 1)
          } else if (output == 1) {
            // moved to a valid spot, move more and revert move
            val path = shortestPath(newCoord, newVisited, newPath)
            computer.acceptInput(List(opposites.get(direction).get.id))
            path
          } else {
            walls.add(newCoord)
            None
          }
          result
        }).minOption
    }

    shortestPath(Coordinate(0, 0), Set(), List()).get
  }

  def oxygenFillTime(spaces: Set[Coordinate], oxygen: Coordinate): Int = {
    var edgeSpaces = Set(oxygen)
    val filledSpaces = mutable.Set[Coordinate](oxygen)
    var time = 0
    while (filledSpaces.size != spaces.size) {
      edgeSpaces = edgeSpaces.flatMap( location =>
        List(
          (Coordinate(location.x, location.y + 1)),
          (Coordinate(location.x, location.y - 1)),
          (Coordinate(location.x + 1, location.y)),
          (Coordinate(location.x - 1, location.y)))
      ).filter(neighbor => !filledSpaces.contains(neighbor) && spaces.contains(neighbor)).toSet

      filledSpaces.addAll(edgeSpaces)
      printSpaces(spaces, filledSpaces)
      time += 1
    }
    time
  }

  def printSpaces(spaces: Set[Coordinate], filledSpaces: mutable.Set[Coordinate]) = {
    println("---------------------------------------------------------")
    var (maxX, minX, maxY, minY) = (0,0,0,0)
    spaces.foreach(space => {
      if (space.x > maxX) {
        maxX = space.x
      }
      if (space.y > maxY) {
        maxY = space.y
      }
      if (space.x < minX) {
        minX = space.x
      }
      if (space.y < minY) {
        minY = space.y
      }
    })

    (minY to maxY).map(y => (minX to maxX).map(x => {
      if (filledSpaces.contains(Coordinate(x,y))) {
        'X'
      } else if (spaces.contains(Coordinate(x,y))) {
        'O'
      } else {
        ' '
      }
    }).toArray).reverse.foreach(row => println(row.mkString))
  }

  def mapArea(computer: IntComputer): (Set[Coordinate], Coordinate) = {
    val spaces = mutable.Set[Coordinate]()
    val walls: mutable.Set[Coordinate] = mutable.Set()
    var oxygen = Coordinate(0,0)

    def mapArea(location: Coordinate): Unit= {
      val neighbors = List(
        (Direction.North, Coordinate(location.x, location.y + 1)),
        (Direction.South, Coordinate(location.x, location.y - 1)),
        (Direction.East, Coordinate(location.x + 1, location.y)),
        (Direction.West, Coordinate(location.x - 1, location.y)))

      neighbors
        .foreach(neighbor => {
          val (direction, newCoord) = neighbor

          if (!spaces.contains(newCoord) && !walls.contains(newCoord)) {
            // move in that direction
            val output = computer.acceptInput(List(direction.id))._1(0)

            if (output != 0) {
              // moved to a valid spot, move more and revert move
              spaces.add(newCoord)
              if (output == 2) {
                oxygen = newCoord
              }
              mapArea(newCoord)
              computer.acceptInput(List(opposites.get(direction).get.id))
            } else {
              walls.add(newCoord)
            }
          }
        })
    }
    mapArea(Coordinate(0,0))
    (spaces.toSet, oxygen)
  }
}

