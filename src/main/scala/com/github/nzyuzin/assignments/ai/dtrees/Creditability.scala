package com.github.nzyuzin.assignments.ai.dtrees

object Creditability extends Enumeration {
  type Creditability = Value

  val None, Low, Average, High = Value

  def getCreditabilityFrom(creditabilityLevel: Int): Creditability = {
    if (0.equals(creditabilityLevel)) {
      Creditability.None
    } else if (1.equals(creditabilityLevel)) {
      Creditability.Low
    } else if (2.equals(creditabilityLevel)) {
      Creditability.Average
    } else if (3.equals(creditabilityLevel)) {
      Creditability.High
    } else {
      throw new IllegalArgumentException
    }
  }

}
