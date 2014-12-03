package com.github.nzyuzin.assignments.ai.kmeans

import java.awt.{Graphics, Frame, Color, Canvas}
import java.util.Scanner

import scala.collection.mutable
import scala.io.Source

object KMeans {

  def displayResult(clustersToParameters: Map[Parameter, Seq[Parameter]], windowTitle: String): Unit = {
    val FRAME_SIZE = 800
    val canvas = new Canvas() {

      override def paint(g: Graphics): Unit = {

        clustersToParameters.foreach({ (pair: (Parameter, Seq[Parameter])) =>
          val color = pair._1.color
          pair._2.foreach({ p =>
            g.setColor(color)
            val x = ((p.first + p.firstLength) / p.firstLength * FRAME_SIZE).toInt
            val y = ((p.second.abs - p.secondLength) * FRAME_SIZE).toInt
            g.setColor(Color.darkGray)
            g.drawLine(0, FRAME_SIZE / 2, FRAME_SIZE, FRAME_SIZE / 2)
            g.drawLine(FRAME_SIZE / 2, 0, FRAME_SIZE / 2, FRAME_SIZE)
            g.setColor(color)
            g.drawOval(x, y, 5, 5)
            g.fillOval(x, y, 5, 5)
          })
          val p = pair._1
          g.drawRect(((p.first + p.firstLength) / p.firstLength * FRAME_SIZE).toInt, ((p.second.abs - p.secondLength) * FRAME_SIZE).toInt, 7, 7)
        })

      }

    }
    canvas.setSize(FRAME_SIZE, FRAME_SIZE)
    canvas.setBackground(Color.white)

    val frame = new Frame()
    frame.setSize(FRAME_SIZE, FRAME_SIZE)
    frame.add(canvas)
    frame.setTitle(windowTitle)
    frame.setVisible(true)
  }

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

    val colorsList = new mutable.MutableList[Color]
    colorsList += Color.black
    colorsList += Color.red
    colorsList += Color.green
    colorsList += Color.blue
    colorsList += Color.magenta
    colorsList += Color.cyan
    colorsList += Color.orange
    colorsList += Color.pink
    colorsList += Color.yellow
    colorsList += Color.darkGray
    val colorsIterator = colorsList.iterator

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
      result += new Parameter(first, firstLen, second, secondLen, third, colorsIterator.next)
    }
    result
  }

  def main(args: Array[String]) = {
    val file = "parameters.txt"
    val input = Source.fromFile(file)
    val inputScanner = new Scanner(input.bufferedReader())
    val amountOfClusters = readAmountOfClusters(inputScanner)
    val parameters = readParameters(inputScanner)
    val clusters = initClusters(parameters, amountOfClusters)
    var clustersToParameters = cluster(parameters, clusters)

    var iterations = 0
    var break = false
    while (!break) {
      displayResult(clustersToParameters, "Iteration " + iterations)
      val error = computeError(clustersToParameters)
      val newCentroids = chooseCentroids(clustersToParameters)
      val newClustersToParameters = cluster(parameters, newCentroids)
      val newError = computeError(newClustersToParameters)
      println(error, newError)
      if (newError > error || math.abs(newError - error) < (newError + error) / 2 * 0.05) {
        break = true
      }
      clustersToParameters = newClustersToParameters
      iterations += 1
    }

    clustersToParameters.foreach((pair: (Parameter, Seq[Parameter])) =>
    println("elements in cluster " + pair._1 + " = " + pair._2.size))
    println("number of iterations = " + iterations)
    displayResult(clustersToParameters, "Iteration " + iterations)
  }

  def initClusters(parameters: Seq[Parameter], amountOfClusters: Int): Map[Parameter, Seq[Parameter]] = {
    val clusterToParameters = new mutable.HashMap[Parameter, Seq[Parameter]]()
    var i = 0
    val parIter = parameters.iterator
    while (i < amountOfClusters) {
      val par = parIter.next()
      clusterToParameters.put(par, new mutable.MutableList[Parameter])
      i += 1
    }
    clusterToParameters.toMap
  }

  def cluster(
               parameters: Seq[Parameter],
               clusterToParameters: Map[Parameter, Seq[Parameter]]
             ): Map[Parameter, Seq[Parameter]] = {
    val result = new mutable.HashMap[Parameter, mutable.MutableList[Parameter]]
    clusterToParameters.foreach((cToP: (Parameter, Seq[Parameter])) => result.put(cToP._1, new mutable.MutableList))

    parameters.foreach(p => {
      val differences = new mutable.HashMap[Double, Parameter]

      clusterToParameters.keys.foreach({ cluster =>
        differences.put(p.differenceFrom(cluster), cluster)
      })

      val minDist = differences.keySet.min(Ordering.Double)
      val closestCluster = differences.get(minDist).get
      result.get(closestCluster).get += p
    })

    result.toMap
  }

  def chooseCentroids(clusterToParameter: Map[Parameter, Seq[Parameter]]): Map[Parameter, Seq[Parameter]] = {
    val result = new mutable.HashMap[Parameter, mutable.MutableList[Parameter]]
    val chooseCentroid = { clustToParams: (Parameter, Seq[Parameter]) =>
      val oldCentroid = clustToParams._1
      val parameters: Seq[Parameter] = clustToParams._2
      var newFirst = 0.0
      var newSecond = 0.0
      var newThird = 0.0

      var firstLen = 0
      var secondLen = 0

      parameters.foreach({ p =>
        newFirst += p.first / parameters.length
        newSecond += p.second / parameters.length
        newThird += { if (p.third) 1.0 / parameters.length else 0 }
        firstLen = p.firstLength
        secondLen = p.secondLength
      })

      new Parameter(newFirst, firstLen, newSecond, secondLen, newThird > 0.5, oldCentroid.color)
    }
    clusterToParameter.foreach({ (clusterToParameters: (Parameter, Seq[Parameter])) =>
       result.put(chooseCentroid(clusterToParameters), new mutable.MutableList[Parameter])
    })

    result.toMap
  }

  def computeError(parametersToClusters: Map[Parameter, Seq[Parameter]]): Double = {
    var error = 0.0
    parametersToClusters.foreach((clusterToParameters: (Parameter, Seq[Parameter])) =>
      error += clusterToParameters._2.foldLeft(0.0) { (z, p: Parameter) =>
        z + p.differenceFrom(clusterToParameters._1)
      }
    )
    error
  }

}

class Parameter(f: Double, fL: Int, s: Double, sL: Int, t: Boolean, c: Color) {

  def first = f
  def firstLength = fL
  def second = s
  def secondLength = sL
  def third = t

  def color = c

  override def toString: String = {
    "[%.2f %.2f %b]".format(first, second, third)
  }

  def differenceFrom(another: Parameter): Double = {
    euclideanDistance(another)
  }

  def euclideanDistance(another: Parameter): Double = {
    val square = (x: Double) => x * x
    math.sqrt(square((this.first - another.first) / firstLength)
      + square((this.second - another.second) / secondLength)
      + (if (this.third == another.third) 1  else 0))
  }

  def manhattanDistance(another: Parameter): Double = {
    val square = (x: Double) => x * x
    (this.first - another.first).abs / firstLength
      + (this.second - another.second).abs / secondLength
      + (if (this.third == another.third) 1  else 0)
  }

}
