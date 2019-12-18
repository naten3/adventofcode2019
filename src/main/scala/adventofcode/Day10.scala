package adventofcode

import adventofcode.Day10.relativeAsteroids

import scala.io.Source

object Day10 {
  case class Coordinate(x: Int, y: Int)
  def main(args: Array[String]): Unit = {
    val asteroids = Source.fromResource("day10.txt").getLines.map(_.map( _ match {
      case '.' => false
      case '#' => true
    }))
    val asteroidCoordinates = asteroids.zipWithIndex.flatMap(rowWithIndex => {
      val (row, rowNumber) = rowWithIndex
      row.zipWithIndex.filter(_._1).map(_._2).map(Coordinate(_, rowNumber))
    }).toArray
    println(part1(asteroidCoordinates))
    println(part2(asteroidCoordinates))
  }
  def part1(asteroidCoordinates: Array[Coordinate]): Int = {
    relativeAsteroids(asteroidCoordinates)._2.size
  }

  case class FoldResult(lists: Array[List[Coordinate]], position: Int, lastDestroyed: Coordinate)

  def part2(asteroidCoordinates: Array[Coordinate]): Int = {
    val (lazerCoord, asteroidMap) = relativeAsteroids(asteroidCoordinates)
    // all lines of angles in order in which lazer will hit them, asteroids in the same line are sorted by distance from the lazer
    val lazerOrderList = asteroidMap.toArray.sortBy(_._1)
      .map(_._2.sortBy(asteroidCoord => Math.abs(lazerCoord.x - asteroidCoord.x) + Math.abs(lazerCoord.y - asteroidCoord.y))).toArray

    val listLength = lazerOrderList.length
    val lastDestroyed = (1 to 200).foldLeft(FoldResult(lazerOrderList, 0, lazerCoord))((foldResult, index) => {
      var nextIndex = foldResult.position
      var nextAsteroid: Option[Coordinate] = None
      val asteroidLists = foldResult.lists
      while(nextAsteroid.isEmpty) {
        if (asteroidLists(nextIndex).nonEmpty) {
          nextAsteroid = Some(asteroidLists(nextIndex).head)
        } else {
          nextIndex = (nextIndex + 1) % listLength
        }
      }
      val incrementedIndex = (nextIndex + 1) % listLength
      FoldResult(asteroidLists.updated(nextIndex, asteroidLists(nextIndex).tail), incrementedIndex, nextAsteroid.get)
    }).lastDestroyed
    100 * lastDestroyed.x + lastDestroyed.y
  }

  /*
   Finds the asteroid which can see the most other asteroids,
   returns all other asteroids grouped by relative clockwise angle off y axis
   */
  def relativeAsteroids(asteroidCoordinates: Array[Coordinate]): (Coordinate, Map[Double, List[Coordinate]]) = {
    asteroidCoordinates.map(coord => {
      val angleMap = asteroidCoordinates.filter(_ != coord).foldLeft(Map[Double, List[Coordinate]]())((angleMap, newCoord) => {
        //range is -180 to 180
        val degreesClockwiseFromYAxis = Math.toDegrees(Math.atan2(newCoord.x - coord.x, coord.y - newCoord.y))
        val normalizedDegree = if (degreesClockwiseFromYAxis < 0) degreesClockwiseFromYAxis + 360 else  degreesClockwiseFromYAxis
        val asteroidsOnAngleList = angleMap.getOrElse(normalizedDegree, List())
        angleMap + (normalizedDegree -> (newCoord :: asteroidsOnAngleList))
      })
      (coord, angleMap)
    }).maxBy(_._2.size)
  }
}

