package com.github.nzyuzin.assignments.ai.dtrees

object LevelOfTrust extends Enumeration {

  type LevelOfTrust = Value

  val None, Low, Average, High = Value

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
