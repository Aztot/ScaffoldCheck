package com.internal.domain

case class CompareSecondEntity(lstNum: String, chemIds: List[String])

object CompareSecondEntity {
  def fromString(line: String): CompareSecondEntity = {
    line.split('\t').toList match {
      case lst :: chemId :: Nil =>
        CompareSecondEntity(lst, chemId.split(",").toList)
      case any =>
        println(any)
        throw new Exception()
    }
  }
}
