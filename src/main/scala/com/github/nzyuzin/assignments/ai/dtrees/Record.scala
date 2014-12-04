package com.github.nzyuzin.assignments.ai.dtrees

import com.github.nzyuzin.assignments.ai.dtrees.Creditability.Creditability
import com.github.nzyuzin.assignments.ai.dtrees.Education.Education
import com.github.nzyuzin.assignments.ai.dtrees.LevelOfTrust.LevelOfTrust

class Record(edu: Education,
             ag: Int,
             marr: Boolean,
             trust: LevelOfTrust,
             cancer: Int,
             creditable: Creditability = null) {
  def education = edu
  def age = ag
  def married = marr
  def levelOfTrust = trust
  def riskOfCancer = cancer
  def creditability = creditable

  override def toString: String = {
    "[%s %s %s %s %s %s]".format(education, age, married, levelOfTrust, riskOfCancer, creditability)
  }

  override def equals(other: Any): Boolean =
    other match {

      case that: Record =>
        (that canEqual this) &&
          education == that.education &&
          age == that.age &&
          married == that.married &&
          levelOfTrust == that.levelOfTrust &&
          riskOfCancer == that.riskOfCancer &&
          creditability == that.creditability

      case _ => false
    }

  def canEqual(other: Any): Boolean =
    other.isInstanceOf[Record]

  override def hashCode: Int =
    41 * (
      41 * (
        41 *  (
          41 * (
            41 * (
                41 + education.hashCode()
              ) + creditability.hashCode()
            ) + age
          ) + married.hashCode()
        ) + levelOfTrust.hashCode()
      ) + riskOfCancer.hashCode()

}

