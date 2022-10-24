package com.internal.domain

case class CompareEntity(fCode: String, chemId: String)

object CompareEntity {
  def fromString(line: String): CompareEntity = {
    line.split(';').toList match {
      case fCode :: chemId :: Nil =>
        CompareEntity(fCode, chemId)
      case any =>
        println(any)
        throw new Exception()
    }
  }
}
