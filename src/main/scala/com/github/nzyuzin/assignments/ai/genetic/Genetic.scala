package com.github.nzyuzin.assignments.ai.genetic

import java.lang.StringBuilder
import java.util.ArrayList
import collection.JavaConversions._

import java.util.Collections
import java.util.List

object Genetic {

  val LOOK_FOR_MAX = false
  val MAXMIN = if (LOOK_FOR_MAX)  "maximum" else "minimum"
  val ACCURACY = 0.01

  /**
   * Количество итераций при неизменяющемся максимуме.
   * Если максимум не меняется на число, большее ACCURACY,
   * несколько (а именно MAX_EQUALS) итераций подряд, алгоритм
   * завершается.
   */
  val MAX_EQUALS = 10
  val MIN = -10D
  val MAX = 10D

  val geneAmount = bin(toInt(MAX - MIN)).length()
  // Math.pow(x, ...) - здесь x - вероятность, что популяция будет содержать все необходимые для рекомбинации элементы
  val populationSize = 1 + (1D / (1D - Math.pow(0.8, 1D / geneAmount.toDouble))).toInt

  /**
   * Результат скрещивания (2 потомка)
   */
  class CrossbreedingResult(ch1: Int, ch2: Int) {
    def child1 = ch1
    def child2 = ch2
  }

  /**
   * Особь (значение и приспособленность)
   */
  private class Individual(valu: Int, func: Double) extends Comparable[Individual] {

    def value = valu
    def function = func

    override def compareTo(o: Individual): Int = {
      o.function.compareTo(this.function)
    }
  }

  private def function(x: Double): Double = {
    Math.tan(x) + 9 * x * x
  }

  private def callFunction(x: Double): Double = {
    function(x + MIN)
  }

  def main(args: Array[String]) {
    startGA()
  }

  private def startGA() {
//    writeln("Для перевода чисел в код Грея будем умножать их на 100 и округлять до целых чисел.")
//    if (MIN != GA_MIN) {
//      writeln("Чтобы переводить отрицательные числа в код Грея,")
//      writeln("увеличим все значения на " + format(-MIN) + ".")
//      writeln("При подстановке значений в функцию будем уменьшать их.")
//    }
    var population = new Array[Int](populationSize)
    println("Initial population (size = " + populationSize + "):")
    for (i <- 0 until population.length) {
      val point = Math.random() * (MAX - MIN) + MIN
      population(i) = toInt(toDouble(toInt(point)))
    }
    val function = new Array[Double](population.length)
    var best: Double = 0
    var newBest = callFunction(toDouble(population(0)))
    var bestValue = population(0)
    for (i <- 0 until function.length) {
      val current = callFunction(toDouble(population(i)))
      if (isBetter(current, newBest)) {
        newBest = current
        bestValue = population(i)
      }
      function(i) = current
    }
    printPopulation(population, function)
    println(MAXMIN + ": " + format(newBest) + " (" + format(toRealDouble(bestValue)) + ")")

    var counter = 0
    do {
      best = newBest

      population = newPopulation(population, function)

      for (i <- 0 until function.length) {
        val current = callFunction(toDouble(population(i)))
        if (isBetter(current, newBest)) {
          newBest = current
          bestValue = population(i)
        }
        function(i) = current
      }

      println("New population:")
      printPopulation(population, function)
      println("Current " + MAXMIN + " is: " + format(newBest) + " (" + format(toRealDouble(bestValue)) + ").")
      println("Previous " + MAXMIN + ": " + format(best) + ".")
      if (Math.abs(newBest - best) > ACCURACY) {
        counter = 0
      } else {
        counter += 1
      }
    } while (counter < MAX_EQUALS)
    println("found " + MAXMIN + " : " + format(newBest) + " (" + format(toRealDouble(bestValue)) + ").")
  }

  private def newPopulation(population: Array[Int], function: Array[Double]): Array[Int] = {
    val newPopulation = new ArrayList[Individual]()
    for (i <- 0 until population.length) {
      newPopulation.add(new Individual(population(i), function(i)))
    }

//    writeln("Выбираем родителей.")
    val parents = selectParents(population, function)
    /*
     * Из отобранных для скрещивания родителей случайным образом
     * выбираем двоих и скрещиваем
     */
//    writeln("Начинаем скрещивание.")
    for (i <- 0 until parents.length) {
      print("selected parents: ")
      var p1Index = (Math.random() * parents.length).toInt
      if (p1Index == parents.length) {
        p1Index -= 1
      }
      var p2Index: Int = 0
      do {
        p2Index = (Math.random() * parents.length).toInt
      } while (p2Index == parents.length || p2Index == p1Index)

      println(format(toRealDouble(parents(p1Index))) + ", " + format(toRealDouble(parents(p2Index))) + ".")
      val newChildren = crossbreeding(parents(p1Index), parents(p2Index))
      val child1 = newChildren.child1
      val child2 = newChildren.child2
      println("Descendants: " + format(toRealDouble(child1)) + ", " + format(toRealDouble(child2)))
      newPopulation.add(new Individual(child1, callFunction(toDouble(child1))))
      newPopulation.add(new Individual(child2, callFunction(toDouble(child2))))
    }

    println("Intermediate population:")
    printPopulation(newPopulation)
//    writeln("Составим новую популяцию.")
    selectPopulation(newPopulation)
  }

  private def selectParents(population: Array[Int], function: Array[Double]): Array[Int] = {
    val M = populationSize / 2
    val parents = new Array[Int](M)

    /*
     * Случайно выбираем трёх родителей, из них отбираем для
     * скрещивания самого лучшего. Повторяем M раз.
     */

//    writeln("Случайно выбираем трёх особей, из них отбираем для скрещивания лучшего.\nПовторяем " + M + " раз.")
    val usedIndexes = new Array[Int](parents.length)
    for (i <- 0 until parents.length) {
    val parentCandidates = new Array[Int](3)
      for (j <- 0 until parentCandidates.length) {
        var p: Int = 0
        do {
          p = (Math.random() * (population.length - 1)).toInt
        } while (arrayContains(parentCandidates, p, j) || arrayContains(usedIndexes, p, i))
        parentCandidates(j) = p
      }

      val bestIndex = findBestParentIndex(parentCandidates, function)
      parents(i) = population(bestIndex)
      usedIndexes(i) = bestIndex
    }
    println("Selected parents: ")
    printRealDoubleArray(parents)
    parents
  }

  /**
  * Выбираем populationSize новых особей
  *
  * @param newPopulation родители + потомки
  */
  private def selectPopulation(newPopulation: List[Individual]): Array[Int] = {
    selectByDisplacement(newPopulation)
  }

  private def selectByDisplacement(newPopulation: List[Individual]): Array[Int] = {
//    writeln("Отбор вытеснением.")
    val newPopulationArray = new Array[Int](populationSize)
    val usedIndexes = new Array[Int](populationSize)
    val size = newPopulation.size()
    val index = (Math.random() * (size - 1)).toInt
    newPopulationArray(0) = newPopulation.get(index).value
    usedIndexes(0) = index
    for (i <- 1 until populationSize) {
      val it = newPopulation.listIterator()
      var bestIndex = 0
      var individual = it.next()
      while (arrayContains(usedIndexes, bestIndex, i)) {
        it.next()
        bestIndex += 1
      }
      var max = calculatePopulationDiff(individual.value, newPopulationArray, i).toDouble
      var j = bestIndex
      while (it.hasNext) {
        val current = it.next()
        val displacement = calculatePopulationDiff(current.value, newPopulationArray, i).toDouble
        if (displacement > max) {
          max = displacement
          individual = current
          bestIndex = j
        }
        j += 1
      }
      newPopulationArray(i) = individual.value
      usedIndexes(i) = bestIndex
    }
    newPopulationArray
  }

  private def calculatePopulationDiff(individual: Int, newPopulation: Array[Int], endIndex: Int): Int = {
    if (endIndex == 0) {
      throw new IllegalArgumentException("endIndex can't be zero")
    }
    var min = compareGray(individual, newPopulation(0))
    for (i <- 1 until endIndex) {
      val compare = compareGray(individual, newPopulation(i))
      if (compare < min) {
        min = compare
      }
    }
    min
  }

  private def findBestParentIndex(parentCandidates: Array[Int], function: Array[Double]): Int = {
    var index = parentCandidates(0)
    var bestFunction = function(0)
    for (i <- 0 until parentCandidates.length) {
      if (isBetter(function(parentCandidates(i)), bestFunction)) {
        index = parentCandidates(i)
        bestFunction = function(parentCandidates(i))
      }
    }
    index
  }

  private def isBetter(f1: Double, f2: Double): Boolean = {
    if (LOOK_FOR_MAX) {
      f1 > f2
    } else {
      f1 < f2
    }
  }

  private def arrayContains(array: Array[Int], value: Int, endIndex: Int): Boolean = {
    for (i <- 0 until endIndex) {
      if (array(i) == value) {
        return true
      }
    }
    false
  }

  private def toInt(value: Double): Int = {
    (value * 100D).toInt
  }

  private def toRealDouble(value: Int): Double = {
    toDouble(value) + MIN
  }

  private def toDouble(value: Int): Double = {
    value.toDouble / 100
  }

  private def grayCode(value: Int): Int = {
    value ^ (value >> 1)
  }

  private def grayDecode(gray: Int): Int = {
    var bin = 0
    var grayVar = gray
    while (grayVar != 0) {
      bin ^= grayVar
      grayVar >>= 1
    }
    bin
  }

  private def compareGray(value1: Int, value2: Int): Int = {
    val c1 = bin(grayCode(value1)).toCharArray
    val c2 = bin(grayCode(value2)).toCharArray
    var dif = 0
    for (i <- 0 until c1.length) {
      if (c1(i) != c2(i)) {
        dif += 1
      }
    }
    dif
  }

  private def bin(value: Int): String = {
    val b = new StringBuilder(Integer.toString(value, 2))
    while (b.length < geneAmount) {
      b.insert(0, "0")
    }
    b.toString
  }

  private def crossbreeding(parent1: Int, parent2: Int): CrossbreedingResult = {
    val val1 = bin(grayCode(parent1))
    val val2 = bin(grayCode(parent2))
    val len = val1.length()
    if (len == 1) {
      throw new IllegalArgumentException("Can't cross only one genome")
    }
    println("Gray code for parents: ")
    println(val1)
    println(val2)
    print("Crossing point: ")
    var point = (Math.random() * len.toDouble).toInt
    if (point >= len) {
      point -= 1
    }
    if (point == 0) {
      point += 1
    }
    println(point)
    var child1 = new StringBuilder()
    var child2 = new StringBuilder()
    child1.append(val1, 0, point)
    child1.append(val2, point, len)
    child2.append(val2, 0, point)
    child2.append(val1, point, len)

    println("Descendants after crossbreeding: ")
    println(child1)
    println(child2)

//    writeln("Проведём мутацию. С вероятностью 0,5 у потомка изменится один ген.")
    child1 = mutation(child1)
    child2 = mutation(child2)

    println("Descendants after mutation: ")
    println(child1)
    println(child2)

    val intChild1 = grayDecode(Integer.parseInt(child1.toString, 2))
    val intChild2 = grayDecode(Integer.parseInt(child2.toString, 2))

    new CrossbreedingResult(intChild1, intChild2)
  }

  private def mutation(value: StringBuilder): StringBuilder = {
    if (Math.random() < 0.5) {
      return value
    }
    val position = (Math.random() * (value.length - 1).toDouble).toInt
    val c = value.charAt(position)
    value.replace(position, position + 1, (c.toString.toInt ^ 1).toString)
  }

  private def format(value: Double): String = {
    value.formatted("%.3f")
  }

  private def printRealDoubleArray(array: Array[Int]): Unit = {
    val b = new StringBuilder()
    for (d: Int <- array) {
      b.append(format(toRealDouble(d))).append("\n")
    }
    print(b)
  }

  private def printPopulation(population: Array[Int], function: Array[Double]): Unit = {
    for (i <- 0 until population.length) {
      println("f(" + format(toRealDouble(population(i))) + ") = " + format(function(i)))
    }
  }

  private def printPopulation(population: List[Individual]): Unit = {
    for (ind <- population) {
      println("f(" + format(toRealDouble(ind.value)) + ")  = " + format(ind.function))
    }
  }
}
