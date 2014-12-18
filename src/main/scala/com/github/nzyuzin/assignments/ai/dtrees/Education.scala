package com.github.nzyuzin.assignments.ai.dtrees

object Education extends Enumeration {
  type Education = Value

  val Secondary, Vocational, Undergraduate, Graduate, Doctor = Value

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

}
