package adventofcode

import scala.io.Source

object Day14 {
  val recipePat = "(.*) => ([0-9]+) ([A-Z]+)".r

  case class Chemical(amount: Long, symbol: String)

  case class Recipe(ingredients: Array[Chemical], result: Chemical)

  def main(args: Array[String]): Unit = {
    val recipes = parseInput

    println(part1(recipes))
    println(part2(recipes))
  }

  def parseInput(): Array[Recipe] = {
    Source.fromResource("day14.txt").getLines.map(line => {
    val recipePat(ingredientString, resultCount, resultSymbol) = line
    val ingredients = ingredientString.split(",").map(s => {
      val Array(ingredientCount: String, symbol: String) = s.trim.split(' ')
      Chemical(ingredientCount.toLong, symbol)
    })

    Recipe(ingredients, Chemical(resultCount.toLong, resultSymbol))
     }).toArray
  }

  def part1(recipes: Array[Recipe]): Long = {
    cheapestFuel(recipes, 1L).cost
  }

  def part2(recipes: Array[Recipe]): Long = {
    var low = 800000L
    var high = 3000000L

    var numNeeded = 0L
    // this is inefficient :/
    while(high - low > 1) {
      numNeeded = (high + low) / 2
      val fuelRequired = cheapestFuel(recipes, numNeeded).cost
      if (fuelRequired > 1000000000000L ) {
        high = numNeeded
      } else {
        low = numNeeded
      }
    }
    Math.min(low, high)
  }

  case class SynthesisResult(cost: Long, leftovers: Map[String, Long])

  def cheapestFuel(recipes: Array[Recipe], numNeeded: Long): SynthesisResult = {
    val recipeMap = recipes.groupBy(_.result.symbol).view.mapValues(_ (0))

    def cheapestFuel(symbol: String, numNeeded: Long, leftovers: Map[String, Long]): SynthesisResult = {
      if (numNeeded == 0) {
        SynthesisResult(0, leftovers)
      }
      if (symbol == "ORE") {
        SynthesisResult(numNeeded, leftovers)
      } else {
        val (numNeededAfterLeftovers, newLeftovers) = {
          val useableLeftovers = leftovers.get(symbol)
          if (useableLeftovers.isDefined && useableLeftovers.get > 0) {
            val usedLeftovers = Math.min(useableLeftovers.get, numNeeded)
            val remainingLeftovers = useableLeftovers.get - usedLeftovers
            (numNeeded - usedLeftovers, leftovers + (symbol -> remainingLeftovers))
          } else {
            (numNeeded, leftovers)
          }
        }


        val recipe = recipeMap.get(symbol).get
        val numBatches = Math.ceil(numNeededAfterLeftovers.toFloat / recipe.result.amount).toLong

        val result = recipe.ingredients.foldLeft(SynthesisResult(0, newLeftovers))((synthesisResult, ingredient) => {
          val numIngredientNeeded = ingredient.amount * numBatches
          val ingredientSynthesisResult = cheapestFuel(ingredient.symbol, numIngredientNeeded, synthesisResult.leftovers)
          SynthesisResult(synthesisResult.cost + ingredientSynthesisResult.cost, ingredientSynthesisResult.leftovers)
        })
        val leftoverProduct = numBatches * recipe.result.amount - numNeededAfterLeftovers
        val totalLeftoverProduct = result.leftovers.getOrElse(symbol, 0L) + leftoverProduct
        SynthesisResult(result.cost, result.leftovers + (symbol -> totalLeftoverProduct))
      }
    }
    cheapestFuel("FUEL", numNeeded, Map())
  }
}

