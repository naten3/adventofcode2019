package adventofcode

import adventofcode.Day6.{directOrbitMap, indirectOrbits}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day6 {
  val orbitPattern = "([A-Z0-9]+)\\)([A-Z0-9]+)".r
  def main(args: Array[String]): Unit = {
    val orbits = Source.fromResource("day6.txt").getLines.map(line => {
      val orbitPattern(oribtee, orbiter) = line;
      OrbitRelationship(oribtee, orbiter)
    }).toArray
    println(part1(orbits))
    println(part2(orbits))
  }

  case class OrbitRelationship(orbitee: String, orbiter: String)
  case class OrbitCounts(totalOrbits: Int, totalPlanets: Int);

  def part1(orbits: Array[OrbitRelationship]): Int = {
    indirectOrbits(directOrbitMap(orbits))
  }

  def directOrbitMap(orbits: Array[OrbitRelationship]): Map[String, List[String]] = {
    orbits.foldLeft(Map[String, List[String]]())(( oMap, newOrbit) => {
      val orbitList = oMap.getOrElse(newOrbit.orbitee, List())
      oMap + (newOrbit.orbitee -> (newOrbit.orbiter :: orbitList))
    })
  }

  def indirectOrbits(directOrbitMap: Map[String, List[String]]): Int = {
    val indirectOrbitMap = mutable.Map[String, OrbitCounts]()

    def indirectOrbits(planet: String): OrbitCounts = {
      val cachedValue = indirectOrbitMap.get(planet)
      if (cachedValue.isDefined) {
        return cachedValue.get
      }
      val indirectOrbitCount = directOrbitMap.getOrElse(planet, List()).foldLeft(OrbitCounts(0,0))((orbitCounts, newOrbiter) => {
        val newOrbiterCounts = indirectOrbits(newOrbiter)
        val newTotalPlanets = orbitCounts.totalPlanets + newOrbiterCounts.totalPlanets
        // add the orbits of the children, and also the total number of planets which are orbitting this new planet
        val newTotalOrbits = orbitCounts.totalOrbits + newOrbiterCounts.totalOrbits + newOrbiterCounts.totalPlanets
        OrbitCounts(newTotalOrbits, newTotalPlanets)
      })

      //add one to planet count for this planet
      val indirectCountsWithSelf = OrbitCounts(indirectOrbitCount.totalOrbits, indirectOrbitCount.totalPlanets + 1)

      indirectOrbitMap.put(planet, indirectCountsWithSelf)
      indirectCountsWithSelf
    }

    directOrbitMap.keys.map( planet => indirectOrbits(planet)).map(_.totalOrbits).max
  }

  def part2(orbits: Array[OrbitRelationship]): Int = {
    minSantaDistance(orbits)
  }

  def minSantaDistance(orbits: Array[OrbitRelationship]): Int = {
    case class MinDistanceResult(santaDistance: Option[Int], youDistance: Option[Int], totalDistance: Option[Int])
    val orbitMap = directOrbitMap(orbits)
    val minDistanceMap = mutable.Map[String, MinDistanceResult]()

    def  minSantaDistance(planet: String) : MinDistanceResult = {
      val minDistanceResult = minDistanceMap.get(planet)
      if (minDistanceResult.isDefined) {
        return minDistanceResult.get
      }
      var youDistance = if (planet == "YOU") Some(0) else None
      var sanDistance = if (planet == "SAN") Some(0) else None
      orbitMap.getOrElse(planet, List()).foreach(orbitter => {
        val orbitterDistanceResult = minSantaDistance(orbitter)
        if(orbitterDistanceResult.totalDistance.isDefined) {
          val result = orbitterDistanceResult
          minDistanceMap.put(planet, result)
          return result;
        }
        if (orbitterDistanceResult.santaDistance.isDefined) {
          sanDistance = orbitterDistanceResult.santaDistance.map(_+1)
        }
        if (orbitterDistanceResult.youDistance.isDefined) {
          youDistance = orbitterDistanceResult.youDistance.map(_+1)
        }
        if (sanDistance.isDefined && youDistance.isDefined) {
          // this is hopefully the closest node common to both
          val result = MinDistanceResult(None, None, Some(sanDistance.get + youDistance.get))
          minDistanceMap.put(planet, result)
          return result;
        }
      })

      val result = MinDistanceResult(sanDistance, youDistance, None)
      minDistanceMap.put(planet, result)
      result;
    }

   val totalDistanceMatch = orbitMap.keys.map(minSantaDistance).find(_.totalDistance.isDefined).map(_.totalDistance.get)
    if (totalDistanceMatch.isEmpty) {
      throw new Exception("no match found")
    }
    // Subtract 2 because we counted the distance from YOU to its planet and SAN to its planet
    totalDistanceMatch.get - 2
  }


}

