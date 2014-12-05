package com.github.nzyuzin.assignments.ai.dtrees

import com.github.nzyuzin.assignments.ai.dtrees.Creditability.Creditability

class Node(cond: Record => Boolean, condDescr: String, nextNodes: Seq[Node], cred: Creditability, num: Int) {

  def condition = cond
  def subNodes = nextNodes
  def creditability = cred
  def conditionDescription = condDescr

  def number = num

  override def toString: String = {
    if (creditability != null) {
      conditionDescription + ": " + creditability
    } else {
      conditionDescription + ": " + subNodes
    }
  }

}
