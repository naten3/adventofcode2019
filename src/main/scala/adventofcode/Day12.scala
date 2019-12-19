package adventofcode

import adventofcode.Day12.Vector

import scala.io.Source

object Day12 {
  val coordLine = "<x=(-?[0-9]+), y=(-?[0-9]+), z=(-?[0-9]+)>".r
  case class Vector(x: Int, y: Int, z: Int)
  case class PlanetMetrics(position: Vector, velocity: Vector)
  def main(args: Array[String]): Unit = {
    val positions = Source.fromResource("day12.txt").getLines.map( line => {
      val coordLine(x,y,z) = line
      Vector(x.toInt,y.toInt,z.toInt)
    }).toArray

    println(part1(positions))
    println(part2(positions))
  }

  def part1(positions: Array[Vector]): Int = {
    val updatedModel = planetModel(positions, 1000)
    totalEnergy(updatedModel)
  }

  def part2(positions: Array[Vector]): BigInt = {
    val planetMetrics = positions.map(PlanetMetrics(_, Vector(0,0,0)))
    var metrics = planetMetrics
    var iterations = 1
    val periods: Array[Option[BigInt]] = Array(None, None, None)
    val directions: Array[Vector => Int] = Array(v => v.x, v => v.y, v => v.z)

    while(periods.exists(_.isEmpty)) {
      val updatedMetrics = incrementMetrics(metrics)

        directions.zipWithIndex.foreach(di => {
          val (directionFn, idx) = di
          val matching = !planetMetrics.zip(updatedMetrics).exists((ab) => {
            val (a,b) = ab
            directionFn(a.velocity) != directionFn(b.velocity) || directionFn(a.position) != directionFn(b.position)
          })

          if (matching) {
            periods(idx) = Some(BigInt(iterations))
          }
        })

      iterations += 1
      metrics = updatedMetrics
    }
    val period = lcm(lcm(periods(0).get, periods(1).get), periods(2).get)
    period // this is double the correct answer for some reason?
  }

  def lcm(a: BigInt, b: BigInt): BigInt = a * b / a.gcd(b)

  def planetModel(positions: Array[Vector],  stepCount: Int): Array[PlanetMetrics] = {
    val planetMetrics = positions.map(PlanetMetrics(_, Vector(0,0,0)))
    (1 to stepCount).foldLeft(planetMetrics)((newMetrics, _) => {
      incrementMetrics(newMetrics)
    })
  }

  def incrementMetrics(metrics: Array[PlanetMetrics]): Array[PlanetMetrics] = {
    val updatedVelocities = newVelocities(metrics);
    val positions = metrics.map(_.position)
    val updatedPositions = newPositions(positions, updatedVelocities)
    updatedPositions.zip(updatedVelocities).map(pWithV => {
      val (pos, vel) = pWithV
      PlanetMetrics(pos, vel)
    })
  }

  def newVelocities(planetMetrics: Array[PlanetMetrics]): Array[Vector] = {
    val velocities = planetMetrics.map(_.velocity)
    val pairs = for {
      (x, idxX) <- planetMetrics.zipWithIndex
      (y, idxY) <- planetMetrics.zipWithIndex
      if idxX < idxY
    } yield ((x, idxX), (y, idxY))
    pairs.foldLeft(velocities)((velocities, pair) => {
      val ((a: PlanetMetrics, aIndex), (b: PlanetMetrics,bIndex)) = pair
      val (aPos, bPos) = (a.position, b.position)
      // Determine the change b influences on a along each axis
      val Array(aXChange, aYChange, aZChange) = Array((aPos.x, bPos.x), (aPos.y, bPos.y), (aPos.z, bPos.z)).map(positionDirection => {
        val (aP, bP) = positionDirection
        if (aP < bP) 1 else if (aP > bP) -1 else 0
      })

      // calculate the changes on A and B
      velocities.zipWithIndex.map( velWithIdx => {
        val (vel, idx) = velWithIdx
        if (idx == aIndex) {
          Vector(vel.x + aXChange, vel.y + aYChange, vel.z + aZChange)
        } else if (idx == bIndex) {
          Vector(vel.x - aXChange, vel.y - aYChange, vel.z - aZChange)
        } else {
          vel
        }
      })
    })
  }

  def newPositions(positions: Array[Vector], velocities: Array[Vector]): Array[Vector] = {
    positions.zip(velocities).map( pWithV => {
      val (pos, vel) = pWithV
      Vector(pos.x + vel.x, pos.y + vel.y, pos.z + vel.z)
    })
  }

  def totalEnergy(planetMetrics: Array[PlanetMetrics]): Int = {
    planetMetrics.map(metric => {
      Array(metric.position, metric.velocity)
        .map((vec: Vector) => Array(vec.x, vec.y, vec.z).map(Math.abs(_)).sum).product
    }).sum
  }
}

