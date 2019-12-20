package adventofcode

import scala.io.Source

object Day14 {
  val recipePat = "(.*) => ([0-9]+) ([A-Z]+)".r

  case class Chemical(amount: Int, symbol: String)

  case class Recipe(ingredients: Array[Chemical], result: Chemical)

  def main(args: Array[String]): Unit = {
    val recipes = parseInput

    println(part1(recipes))
  }

  def parseInput(): Array[Recipe] = {
    Source.fromResource("day14.txt").getLines.map(line => {
    val recipePat(ingredientString, resultCount, resultSymbol) = line
    val ingredients = ingredientString.split(",").map(s => {
      val Array(ingredientCount: String, symbol: String) = s.trim.split(' ')
      Chemical(ingredientCount.toInt, symbol)
    })

    Recipe(ingredients, Chemical(resultCount.toInt, resultSymbol))
     }).toArray
  }

  def part1(recipes: Array[Recipe]): Int = {
    cheapestFuel(recipes)
  }

  case class SynthesisResult(cost: Int, leftovers: Map[String, Int])
  def cheapestFuel(recipes: Array[Recipe]): Int = {
    val recipeMap = recipes.groupBy(_.result.symbol)
    def cheapestFuel(currentlyMaking: String, currentCost: Int, ): Int = {
      val recipes = recipeMap(currentlyMaking)

      val minOre = recipes.map(recipe => {
        recipe.ingredients.map( ingredient => {
          if (ingredient.symbol == "ORE") {
            ingredient.amount
          } else {

          }
        }).sum
      }).min
    }

    cheapestFuel("FUEL", 0, Map())
  }
}

