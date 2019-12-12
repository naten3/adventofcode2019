package adventofcode

import adventofcode.Day3.Direction.Direction
import adventofcode.Day3.Orientation.Orientation

import scala.io.Source
import scala.math.{abs, max, min}

object Day3 {
  def main(args: Array[String]): Unit = {
    val wires = Source.fromResource("day3.txt").getLines.map(_.split(',').map(toWire).toList).toList
    println(part1(wires))
    println(part2(wires))
  }

  val wireSectionPattern = "([A-Z])([0-9]+)".r

  object Direction extends Enumeration {
    type Direction = Value
    val Up,Down,Left,Right = Value
  }

  object Orientation extends Enumeration {
    type Orientation = Value
    val Hor, Vert = Value
  }

  case class WireSection(direction: Direction, length: Int)
  case class Coord(x: Int, y: Int)
  case class Segment(orientation: Orientation, start: Coord, end: Coord, stepsAway: Int)

  def toWire(s: String): WireSection = {
    val wireSectionPattern(direction, length) = s
    val d = direction match {
      case "U" => Direction.Up
      case "D" => Direction.Down
      case "L" => Direction.Left
      case "R" => Direction.Right
    }
    WireSection(d, length.toInt)
  }

  def part1(lines: List[List[WireSection]]): Int = {
    val segments = lines.map(createSegments)
    segments(0).flatMap(x => segments(1).map(y => intersection(x,y))).flatten.map(inter => abs(inter._1.x) + abs(inter._1.y)).min
  }

  def part2(lines: List[List[WireSection]]): Int = {
    val segments = lines.map(createSegments)
    segments(0).flatMap(x => segments(1).map(y => intersection(x,y).map(_._2))).flatten.min
  }

  def createSegments(wire: List[WireSection]): List[Segment] = {
    case class ReduceResult(coord: Coord, segments: List[Segment], startStep: Int)
    wire.foldLeft(ReduceResult(Coord(0,0), List(), 0))( (reduceResult, wireSection) => {
      var orientation: Orientation = null
      var newX = reduceResult.coord.x
      var newY = reduceResult.coord.y

      if( wireSection.direction == Direction.Up || wireSection.direction == Direction.Down) {
        orientation = Orientation.Vert
        newY = if(wireSection.direction == Direction.Up) reduceResult.coord.y + wireSection.length else reduceResult.coord.y - wireSection.length
      } else {
        orientation = Orientation.Hor
        newX = if(wireSection.direction == Direction.Right) reduceResult.coord.x + wireSection.length else reduceResult.coord.x - wireSection.length
      }
      val segment = Segment(orientation, reduceResult.coord, Coord(newX, newY), reduceResult.startStep)
      ReduceResult(Coord(newX, newY), segment :: reduceResult.segments, reduceResult.startStep + wireSection.length)
    })
  }.segments

  //returns coord and sum of steps
  def intersection(a: Segment, b: Segment): Option[(Coord, Int)] = {
    if (a.orientation== b.orientation) {
      // hopefully wires the same direction can't overlap
      None
    } else {
      val hor = if(a.orientation == Orientation.Hor) a else b
      val vert = if(hor == a) b else a
      if (between(hor.start.x, hor.end.x, vert.start.x) && between(vert.start.y, vert.end.y, hor.start.y)) {
        val totalSteps = a.stepsAway + b.stepsAway + abs(hor.start.x - vert.start.x) + abs(vert.start.y - hor.start.y)
        Some((Coord(vert.start.x, hor.start.y), totalSteps))
      } else {
        None
      }
    }
  }

  def between(bound1: Int, bound2: Int, v: Int): Boolean = v >= min(bound1, bound2) && v <= max(bound1, bound2)
}


