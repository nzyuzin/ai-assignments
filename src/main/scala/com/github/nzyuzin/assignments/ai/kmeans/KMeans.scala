package com.github.nzyuzin.assignments.ai.kmeans

import java.util.Scanner

import scala.collection.mutable
import scala.io.Source

object KMeans {

  def readAmountOfClusters(input: Scanner): Int = {
    input.nextInt()
  }

  def readParameters(input: Scanner): Seq[Parameter] = {
    val result = new mutable.MutableList[Parameter]
    val lowerBoundFirst = input.nextInt()
    val upperBoundFirst = input.nextInt()
    val firstLen = upperBoundFirst - lowerBoundFirst
    val lowerBoundSecond = input.nextInt()
    val upperBoundSecond = input.nextInt()
    val secondLen = upperBoundSecond - lowerBoundSecond
    while (input.hasNext) {
      val first = input.nextDouble()
      if (first < lowerBoundFirst || first > upperBoundFirst) {
        throw new RuntimeException("first parameter is out of bounds: " + first)
      }
      val second = input.nextDouble()
      if (second < lowerBoundSecond || second > upperBoundSecond) {
        throw new RuntimeException("second parameter is out of bounds: " + second)
      }
      val third = input.nextBoolean()
      result += new Parameter(first, firstLen, second, secondLen, third)
    }
    result
  }

  def main(args: Array[String]) = {
    val file = "parameters.txt"
    val input = Source.fromFile(file)
    val inputScanner = new Scanner(input.bufferedReader())
    val amountOfClusters = readAmountOfClusters(inputScanner)
    val parameters = readParameters(inputScanner)
    val result = cluster(parameters, amountOfClusters)
    println(result, computeError(result))
  }

  def cluster(parameters: Seq[Parameter], amountOfClusters: Int): Map[Parameter, Seq[Parameter]] = {
    val clusterToParameters = new mutable.HashMap[Parameter, mutable.MutableList[Parameter]]()

    var i = 0
    val parIter = parameters.iterator
    while (i < amountOfClusters) { // Initial setting for clusters
      val par = parIter.next()
      clusterToParameters.put(par, new mutable.MutableList[Parameter])
      i += 1
    }

    parameters.foreach(p => {
      val clustIter = clusterToParameters.keysIterator
      val differences = new mutable.HashMap[Double, Parameter]
      1.to(amountOfClusters).foreach({ i =>
        val clust = clustIter.next()
        differences.put(clust.differenceFrom(p), clust)
      })
      val minDist = differences.keySet.min(Ordering.Double)
      clusterToParameters.get(differences.get(minDist).get).get += p
    })

    clusterToParameters.toMap
  }

  def computeError(parametersToClusters: Map[Parameter, Seq[Parameter]]): Double = {
    var error = 0.0
    parametersToClusters.foreach((parToClust: (Parameter, Seq[Parameter])) =>
      error += parToClust._2.foldLeft(0.0) { (z, p: Parameter) =>
        z + p.differenceFrom(parToClust._1)
      }
    )
    error
  }

}

class Parameter(f: Double, fL: Int, s: Double, sL: Int, t: Boolean) {

  def first() = f
  def firstLength() = fL
  def second() = s
  def secondLength() = sL
  def third() = t

  override def toString: String = {
    first + " " + second + " " + third
  }

  def differenceFrom(another: Parameter): Double = {
    val square = (x: Double) => x * x
    math.sqrt(square((this.first - another.first) / firstLength)
      + square((this.second - another.second) / secondLength)
      + (if (this.third == another.third) 1  else 0))
  }

}
