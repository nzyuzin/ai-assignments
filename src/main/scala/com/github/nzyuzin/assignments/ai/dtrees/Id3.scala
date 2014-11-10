package com.github.nzyuzin.assignments.ai.dtrees

import java.util.Scanner

import com.github.nzyuzin.assignments.ai.dtrees.Education.Education
import com.github.nzyuzin.assignments.ai.dtrees.LevelOfTrust.LevelOfTrust

import scala.collection.mutable
import scala.io.Source

object Id3 {

  def main(args: Array[String]): Unit = {
    val file = "records.txt"
    val input = Source.fromFile(file)
    val inputScanner = new Scanner(input.bufferedReader())
    val records = readRecords(inputScanner)
    records.foreach(r => println(r))
  }

  def readClassifiedRecord(input: Scanner): Seq[Pair[Record, Int]] = {
    null
  }

  def readRecords(input: Scanner): Seq[Record] = {
    val result = new mutable.MutableList[Record]
    while (input.hasNext) {
      val education: Education = getEducationFrom(input.next())

      val age = input.nextInt()
      if (age < 18 || age > 50) {
        throw new RuntimeException("Incorrect age: " + age)
      }
      val married = input.nextBoolean()
      val levelOfTrust = getLevelOfTrustFrom(input.nextInt())
      val riskOfCancer = input.nextDouble()
      if (riskOfCancer < 50 || riskOfCancer > 100) {
        throw new RuntimeException("Incorrect risk of  cancer: " + riskOfCancer)
      }
      result += new Record(education, age, married, levelOfTrust, riskOfCancer)
    }
    result
  }

  def getEducationFrom(educationString: String): Education = {
    if (Education.Secondary.toString.toLowerCase.equals(educationString.toLowerCase)) {
      Education.Secondary
    } else if (Education.Vocational.toString.toLowerCase.equals(educationString.toLowerCase)) {
      Education.Vocational
    } else if (Education.Undergraduate.toString.toLowerCase.equals(educationString.toLowerCase)) {
      Education.Undergraduate
    } else if (Education.Graduate.toString.toLowerCase.equals(educationString.toLowerCase)) {
      Education.Graduate
    } else if (Education.Doctor.toString.toLowerCase.equals(educationString.toLowerCase)) {
      Education.Doctor
    } else {
      null
    }
  }

  def getLevelOfTrustFrom(levelOfTrustInt: Int): LevelOfTrust = {
    if (0.equals(levelOfTrustInt)) {
      LevelOfTrust.None
    } else if (1.equals(levelOfTrustInt)) {
      LevelOfTrust.Low
    } else if (2.equals(levelOfTrustInt)) {
      LevelOfTrust.Average
    } else if (3.equals(levelOfTrustInt)) {
      LevelOfTrust.High
    } else {
      null
    }
  }

}