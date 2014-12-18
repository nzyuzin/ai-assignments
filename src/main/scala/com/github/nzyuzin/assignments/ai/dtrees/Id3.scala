package com.github.nzyuzin.assignments.ai.dtrees

import java.util.Scanner

import com.github.nzyuzin.assignments.ai.dtrees.Creditability.Creditability
import com.github.nzyuzin.assignments.ai.dtrees.Education.Education

import scala.collection.mutable
import scala.io.Source

object Id3 {

  def main(args: Array[String]): Unit = {
    val algorithm = new Id3Implementation
    val input = Source.fromFile(algorithm.fileName())
    val inputScanner = new Scanner(input.bufferedReader())
    val records = algorithm.readClassifiedRecords(inputScanner)
    val attributes = new mutable.HashMap[String, Record => Any]
    attributes.put("Age", record => record.age)
    attributes.put("Education", record => record.education)
    attributes.put("Level of Trust", record => record.levelOfTrust)
    attributes.put("Married", record => record.married)
    attributes.put("Risk of Cancer", record => record.riskOfCancer)
    val rootNode = algorithm.buildTree(null, "RootNode", attributes.toMap, records)

    algorithm.printTree(rootNode, "")
  }

  class Id3Implementation {

    def fileName(): String = {
      "records_id3.txt"
    }

    def printTree(tree: Node, offset: String) {
      print(offset)
      println(tree.conditionDescription + ":" + " -- " + tree.number)
      if (tree.subNodes == null) {
        println(offset + "\t" + tree.creditability)
      } else {
        tree.subNodes.foreach(node => printTree(node, offset + "\t"))
      }
    }

    var counter: Int = 0

    def buildTree(condition: Record => Boolean, conditionDescription: String,
                  attributes: Map[String, Record => Any], records: Seq[Record]): Node = {
      if (allInSameClass(records)) {
        if (records.isEmpty) {
          return new Node(condition, conditionDescription, null, Creditability.None, counter)
        } else {
          return new Node(condition, conditionDescription, null, records.head.creditability, counter)
        }
      }
      val bestAttr = getAttributeByGain(attributes, records)
      val partition = getPartitionByAttribute(bestAttr._2, bestAttr._1, records)
      val subNodes = new mutable.MutableList[Node]
      val count = counter
      partition.foreach({ conditionToRecords =>
        counter += 1
        val newNode = buildTree(conditionToRecords._1._1, conditionToRecords._1._2, attributes, conditionToRecords._2)
        if (newNode != null) {
          subNodes += newNode
        }
      })
      new Node(condition, conditionDescription, subNodes.toList, null, count)
    }

    def getPartitionByAttribute(attr: Record => Any, attrDescription: String,
                                records: Seq[Record]): Map[(Record => Boolean, String), Seq[Record]] = {
      if (attr(records.head).isInstanceOf[Int] && records.length > 1) {
        val orderedNumbers = getSubsetsByAttribute(attr, records).keys.toList.map(value => value.asInstanceOf[Int]).sorted(Ordering.Int)
        var maxGain: Double = -1
        var bestNumber = -1
        var bestSplit: (Seq[Record], Seq[Record]) = null
        orderedNumbers.take(orderedNumbers.length - 1).foreach({ number =>
          val splitToInfoGain = getThresholdSplitInfoGain(records, attr, number)
          val infoGain = splitToInfoGain._2
          if (maxGain < infoGain) {
            maxGain = infoGain
            bestNumber = number
            bestSplit = splitToInfoGain._1
          }
        })
        val result = new mutable.HashMap[(Record => Boolean, String), Seq[Record]]()
        result.put(({record => attr(record).asInstanceOf[Int] <= bestNumber }, attrDescription + " <= " + bestNumber), bestSplit._1)
        result.put(({record => attr(record).asInstanceOf[Int] > bestNumber }, attrDescription + " > " + bestNumber), bestSplit._2)
        result.toMap
      } else {
        getSubsetsByAttribute(attr, records).map(valueToSubset =>
          (({ record: Record => attr(record) == valueToSubset._1 }, attrDescription + " = " + valueToSubset._1), valueToSubset._2))
      }
    }

    def getThresholdSplitInfoGain(records: Seq[Record], attr: Record => Any, threshold: Int): ((Seq[Record], Seq[Record]), Double) = {
      val lowerThanThreshold = records.filter(record => attr(record).asInstanceOf[Int] <= threshold)
      val greaterThanThreshold = records.filter(record => attr(record).asInstanceOf[Int] > threshold)
      ((lowerThanThreshold, greaterThanThreshold), entropy(records) -
        (lowerThanThreshold.length.toDouble / records.length.toDouble * entropy(lowerThanThreshold) +
          greaterThanThreshold.length.toDouble / records.length.toDouble * entropy(greaterThanThreshold)))
    }

    def allInSameClass(records: Seq[Record]): Boolean = {
      if (records.isEmpty) {
        return true
      }
      val headRecordClass = records.head.creditability
      records.foreach(record => if (record.creditability != headRecordClass) return false)
      true
    }

    def getAttributeByGain(attributes: Map[String, Record => Any], records: Seq[Record]): (String, Record => Any) = {
      var maxGain: Double = -1
      var bestAttribute: (String, Record => Any) = null
      attributes.foreach(attr => {
        val thisGain = getAttributeGain(attr._2, records)
        println("Partition by attribute " + attr._1 + " gain = " + thisGain)
        if (thisGain > maxGain) {
          maxGain = thisGain
          bestAttribute = attr
        }
      })
      println("Best attribute by gain = " + bestAttribute._1 + " for Node " + counter)
      bestAttribute
    }

    def getAttributeGain(attribute: Record => Any, records: Seq[Record]): Double = {
      infoGain(attribute, records)
    }

    def entropy(records: Seq[Record]): Double = {
      val membersInClass = new mutable.HashMap[Creditability, Int]()
      records.foreach(record =>
        membersInClass.put(record.creditability, membersInClass.getOrElse(record.creditability, 0) + 1))
      var result = 0.0
      membersInClass.values.foreach({ amount =>
        val ratio: Double = amount.toDouble / records.length.toDouble
        result += ratio * math.log(ratio) / math.log(2)
      })
      -result
    }

    def getSubsetsByAttribute[T](attribute: Record => T, records: Seq[Record]): Map[T, Seq[Record]] = {
      val result = new mutable.HashMap[T, mutable.MutableList[Record]]()
      records.foreach({ record =>
        val attrValue = attribute(record)
        if (isKnown(attrValue)) {
          if (!result.contains(attrValue)) {
            val newList = new mutable.MutableList[Record]
            newList += record
            result.put(attrValue, newList)
          } else {
            result.get(attribute(record)).get += record
          }
        }
      })
      result.toMap
    }

    def isKnown[T](attrValue: T): Boolean = {
      true
    }

    def infoGain[T](attribute: Record => T, records: Seq[Record]): Double = {
      var sum = 0.0
      val recordsLen = recordsLengthByAttr(attribute, records).toDouble
      getSubsetsByAttribute(attribute, records).values.foreach({ recordsSubset: Seq[Record] =>
        sum += recordsSubset.length.toDouble / recordsLen * entropy(recordsSubset)
      })
      (entropy(records) - sum) * (recordsLen / records.length.toDouble)
    }

    def recordsLengthByAttr[T](attribute: Record => T, records: Seq[Record]): Int = {
      var result = 0
      records.foreach(record => if (isKnown(attribute(record))) result += 1)
      result
    }

    def readClassifiedRecords(input: Scanner): Seq[Record] = {
      val result = new mutable.MutableList[Record]
      while (input.hasNext) {
        val education: Education = Education.getEducationFrom(input.next())

        val age = input.nextInt()
        if ((age < 18 && age != -1) || age > 50) {
          throw new RuntimeException("Incorrect age: " + age)
        }
        val married = input.nextBoolean()
        val levelOfTrust = LevelOfTrust.getLevelOfTrustFrom(input.nextInt())
        val riskOfCancer = input.nextInt()
        if ((riskOfCancer < 50 && riskOfCancer != -1) || riskOfCancer > 100) {
          throw new RuntimeException("Incorrect risk of  cancer: " + riskOfCancer)
        }

        val creditability = Creditability.getCreditabilityFrom(input.nextInt())
        result += new Record(education, age, married, levelOfTrust, riskOfCancer, creditability)
      }
      result
    }

    def readRecords(input: Scanner): Seq[Record] = {
      val result = new mutable.MutableList[Record]
      while (input.hasNext) {
        val education: Education = Education.getEducationFrom(input.next())

        val age = input.nextInt()
        if ((age < 18 && age != -1) || age > 50) {
          throw new RuntimeException("Incorrect age: " + age)
        }
        val married = input.nextBoolean()
        val levelOfTrust = LevelOfTrust.getLevelOfTrustFrom(input.nextInt())
        val riskOfCancer = input.nextInt()
        if ((riskOfCancer < 50 && riskOfCancer != -1) || riskOfCancer > 100) {
          throw new RuntimeException("Incorrect risk of  cancer: " + riskOfCancer)
        }
        result += new Record(education, age, married, levelOfTrust, riskOfCancer)
      }
      result
    }
  }
}
