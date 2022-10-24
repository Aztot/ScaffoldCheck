package com.internal.domain

case class FullEntity(chemId: String, lstNum: String, fCodes: List[String]) {
  override def toString: String = s"$chemId\t$lstNum\t${fCodes.mkString(",")}"
}

object FullEntity {
  def fromString(line: String): FullEntity = {
    line.split('\t').toList match {
      case chemId :: lst :: fCodes :: Nil => FullEntity(chemId, lst, fCodes.split(',').toList)
      case any =>
        println(any)
        throw new Exception()
    }
  }
}
