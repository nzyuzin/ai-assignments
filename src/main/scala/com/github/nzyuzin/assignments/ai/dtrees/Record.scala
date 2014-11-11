package com.github.nzyuzin.assignments.ai.dtrees

import com.github.nzyuzin.assignments.ai.dtrees.Creditability.Creditability
import com.github.nzyuzin.assignments.ai.dtrees.Education.Education
import com.github.nzyuzin.assignments.ai.dtrees.LevelOfTrust.LevelOfTrust

class Record(edu: Education,
             ag: Int,
             marr: Boolean,
             trust: LevelOfTrust,
             cancer: Double,
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
}

