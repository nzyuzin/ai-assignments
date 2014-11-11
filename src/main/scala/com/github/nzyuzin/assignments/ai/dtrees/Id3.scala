package com.github.nzyuzin.assignments.ai.dtrees

import java.util.Scanner

import com.github.nzyuzin.assignments.ai.dtrees.Creditability.Creditability
import com.github.nzyuzin.assignments.ai.dtrees.Education.Education
import com.github.nzyuzin.assignments.ai.dtrees.LevelOfTrust.LevelOfTrust

import scala.collection.mutable
import scala.io.Source

object Id3 {

  def main(args: Array[String]): Unit = {
    val file = "records.txt"
    val input = Source.fromFile(file)
    val inputScanner = new Scanner(input.bufferedReader())
    val records = readClassifiedRecords(inputScanner)
    records.foreach(r => println(r))
  }

  def entropy(records: Seq[Record]): Double = {
    val membersInClass = new mutable.HashMap[Creditability, Int]()
    records.foreach(record =>
      membersInClass.put(record.creditability(), membersInClass.getOrElse(record.creditability(), 0) + 1))
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
      result.getOrElse(attribute(record), new mutable.MutableList[Record]) += record
    })
    result.toMap
  }

  def infoGain[T](attribute: Record => T, records: Seq[Record]): Double = {
    var sum = 0.0
    getSubsetsByAttribute(attribute, records).values.foreach({ recordsSubset: Seq[Record] =>
      sum += recordsSubset.length / records.length * entropy(recordsSubset)
    })
    entropy(records) - sum
  }

  def readClassifiedRecords(input: Scanner): Seq[Record] = {
    val result = new mutable.MutableList[Record]
    while (input.hasNext) {
      val education: Education = Education.getEducationFrom(input.next())

      val age = input.nextInt()
      if (age < 18 || age > 50) {
        throw new RuntimeException("Incorrect age: " + age)
      }
      val married = input.nextBoolean()
      val levelOfTrust = LevelOfTrust.getLevelOfTrustFrom(input.nextInt())
      val riskOfCancer = input.nextDouble()
      if (riskOfCancer < 50 || riskOfCancer > 100) {
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
      if (age < 18 || age > 50) {
        throw new RuntimeException("Incorrect age: " + age)
      }
      val married = input.nextBoolean()
      val levelOfTrust = LevelOfTrust.getLevelOfTrustFrom(input.nextInt())
      val riskOfCancer = input.nextDouble()
      if (riskOfCancer < 50 || riskOfCancer > 100) {
        throw new RuntimeException("Incorrect risk of  cancer: " + riskOfCancer)
      }
      result += new Record(education, age, married, levelOfTrust, riskOfCancer)
    }
    result
  }

}
