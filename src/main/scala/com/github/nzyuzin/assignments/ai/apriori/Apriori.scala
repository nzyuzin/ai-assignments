package com.github.nzyuzin.assignments.ai.apriori

import java.util.Scanner

import scala.collection.mutable
import scala.io.Source

object Apriori {

  def main(args: Array[String]): Unit = {
    val fileName = "apriori.txt"
    val data = readDataFromFile(fileName)
    val frequentSets = getFrequentSets(data, 4)

    frequentSets.foreach({ set =>
      val subsets = set.subsets.filter(subset => subset.nonEmpty && subset.size != set.size)
      subsets.foreach({ subset =>
        val disjoint = set -- subset
        val supportValue = support(subset, disjoint, data)
        val confidenceValue = confidence(subset, disjoint, data)
        println(subset.toString + " -> " + disjoint + " support = " + supportValue + " confidence = " + confidenceValue)
      })
    })

  }

  def support(conditionSet: Set[String], implicationSet: Set[String], data: Seq[Set[String]]): Double = {
    var numberOfMatches = 0

    data.foreach({ transaction =>
      if (conditionSet.intersect(transaction).size == conditionSet.size &&
        implicationSet.intersect(transaction).size == implicationSet.size) {
        numberOfMatches += 1
      }
    })
    numberOfMatches.toDouble / data.size.toDouble
  }

  def confidence(conditionSet: Set[String], implicationSet: Set[String], data: Seq[Set[String]]): Double = {
    var numberOfMatchesBoth = 0
    var numberOfMatchesCondition = 0

    data.foreach({ transaction =>
      if (conditionSet.intersect(transaction).size == conditionSet.size) {
        numberOfMatchesCondition += 1
        if (implicationSet.intersect(transaction).size == implicationSet.size) {
          numberOfMatchesBoth += 1
        }
      }
    })
    numberOfMatchesBoth.toDouble / numberOfMatchesCondition.toDouble
  }

  def getFrequentSets(data: Seq[Set[String]], threshold: Int): Seq[Set[String]] = {
    var itemSet = data.flatten.map({str =>
      val set = new mutable.HashSet[String]()
      set += str
      set.toSet
    }).toSet

    val result = new mutable.MutableList[Set[String]]

    while (true) {
      val frequencies = new mutable.HashMap[Set[String], Int]
      itemSet.foreach({ item =>
        data.foreach({ transaction =>
          if (transaction.intersect(item).size == item.size) {
            frequencies.put(item, frequencies.getOrElse(item, 0) + 1)
          }
        })
      })
      val frequentSets = frequencies.filter(frequency => frequency._2 >= threshold).keys
      if (frequentSets.isEmpty) {
        return result.filter(set => set.size > 1)
      }
      result ++= frequentSets
      val newItemSet = new mutable.HashSet[Set[String]]
      frequentSets.foreach({ set1 =>
        frequentSets.foreach({ set2 =>
          if (set1.size - set1.intersect(set2).size == 1) {
            newItemSet += set1.union(set2)
          }
        })
      })
      itemSet = newItemSet.toSet
    }


    null
  }

  def readDataFromFile(fileName: String): Seq[Set[String]] = {
    val input = Source.fromFile(fileName)
    val inputScanner = new Scanner(input.bufferedReader())
    val result = new mutable.MutableList[Set[String]]
    while (inputScanner.hasNext) {
      val line = inputScanner.nextLine()

      val lineScanner = new Scanner(line)

      val transactionData = new mutable.HashSet[String]()
      while (lineScanner.hasNext) {
        transactionData += lineScanner.next("\\w+")
      }
      result += transactionData.toSet
    }
    result
  }

}
