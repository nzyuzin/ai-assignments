package com.github.nzyuzin.assignments.ai.kmeans

import java.util.Scanner

import scala.collection.mutable
import scala.io.Source

object KMeans {

  def main(args: Array[String]) = {
    val file = "parameters.txt"
    val input = Source.fromFile(file)
    val inputScanner = new Scanner(input.bufferedReader())
    val amountOfClusters = readAmountOfClusters(inputScanner)
    val parameters = readParameters(inputScanner)
    cluster(parameters, amountOfClusters)
  }

  def readAmountOfClusters(input: Scanner): Int = {
    input.nextInt()
  }

  def readParameters(input: Scanner): Seq[Parameter] = {
    val result = new mutable.MutableList[Parameter]
    while (input.hasNext) {
      val first = input.nextDouble()
      val second = input.nextDouble()
      val third = input.nextBoolean()
      result += new Parameter(first, second, third)
    }
    result
  }

  def cluster(parameters: Seq[Parameter], amountOfClusters: Int): Unit = {

  }

}

class Parameter(f: Double, s: Double, t: Boolean) {

  def first() = f
  def second() = s
  def third() = t

  override def toString: String = {
    first + " " + second + " " + third
  }

  def differenceFrom(another: Parameter): Double = {
    val square = (x: Double) => x * x
    math.sqrt(square(this.first - another.first)
      + square(this.second - another.second)
      + (if (this.third == another.third) 1  else 0))
  }

}
