package com.github.nzyuzin.assignments.ai.dtrees

import java.util.Scanner

import com.github.nzyuzin.assignments.ai.dtrees.Id3.Id3Implementation

import scala.collection.mutable
import scala.io.Source

object C45 {

  def main(args: Array[String]): Unit = {
    val algorithm = new C45Implementation
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

  class C45Implementation extends Id3Implementation {
    override def fileName(): String = {
      "records_c45.txt"
    }

    override def getAttributeGain(attribute: (Record) => Any, records: Seq[Record]): Double = {
      infoGain(attribute, records) / splitInfo(attribute, records)
    }

    def splitInfo(attribute: Record => Any, records: Seq[Record]): Double = {
      val attributeValueAmount = new mutable.HashMap[Any, Int]()
      records.foreach({ record =>
        if (isKnown(attribute(record))) {
          attributeValueAmount.put(attribute(record), attributeValueAmount.getOrElse(attribute(record), 0) + 1)
        }
      })
      var result = 0.0
      val recordsLen = recordsLengthByAttr(attribute, records).toDouble
      attributeValueAmount.values.foreach({ amount =>
        val ratio: Double = amount.toDouble / recordsLen
        result += ratio * math.log(ratio) / math.log(2)
      })
      -result
    }

    override def isKnown[T](attrValue: T): Boolean = {
      attrValue != null && attrValue != -1
    }

   }

}
