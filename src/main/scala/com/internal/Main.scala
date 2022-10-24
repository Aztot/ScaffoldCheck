package com.internal

import com.internal.domain._
import com.internal.util._

import java.io.File
import scala.io.Source.fromFile

object Main extends App {

  val fileToSearch = fromFile(new File("src/main/chem_to_id.txt"))
  val secondFileToSearch = fromFile(new File("src/main/LCT_otherChem.txt"))
  val fullFile = fromFile(new File("src/main/Scaffolds.txt"))

  val searchLineIterator = fileToSearch.getLines().map(CompareEntity.fromString).toList

  val secondSearchLineIterator = secondFileToSearch.getLines().map(CompareSecondEntity.fromString).toList

  val entityLineIterator = fullFile.getLines().map(FullEntity.fromString).toList
  val (chemIdRight, chemIdWrong) = tuplePartition {
    entityLineIterator.map {
      case fullEntity@FullEntity(chemId, _, fCodes) =>
        fullEntity -> fCodes.filter(isChemIdFCodeCorrect(_, chemId))
    }.flatMap {
      case (fullEntity, Nil) =>
        List(fullEntity -> false)
      case (fullEntity, fCodes) =>
        fCodes.map(fCode => fullEntity.copy(fCodes = List(fCode)) -> true)
    }
  }
  val (fCodeRight, fCodeWrong) = tuplePartition {
    chemIdWrong.map {
      case fullEntity@FullEntity(_, _, fCodes) =>
        fullEntity -> fCodes.filter(isFCodeCorrect)
    }.flatMap {
      case (fullEntity, Nil) =>
        List(fullEntity -> false)
      case (fullEntity, fCodes) =>
        fCodes.map(fCode => fullEntity.copy(fCodes = List(fCode)) -> true)
    }
  }
  val (lstRight, lstWrong) = fCodeRight.partition {
    case FullEntity(chemId, lstNum, _) =>
      secondSearchLineIterator.exists(isChemIdLstCorrect(_, lstNum, chemId))
  }

  private def isChemIdFCodeCorrect(fCode: String, chemId: String) = {
    searchLineIterator.exists(compareEntity => compareEntity.fCode == fCode && compareEntity.chemId == chemId)
  }

  private def tuplePartition[A](tupleList: List[(A, Boolean)]) = {
    val (rightList, wrongList) = tupleList.partition(_._2)

    (rightList.map(_._1), wrongList.map(_._1))
  }

  private def isFCodeCorrect(fCode: String) = {
    searchLineIterator.exists(compareEntity => compareEntity.fCode == fCode)
  }

  private def isChemIdLstCorrect(compareEntity: CompareSecondEntity, lstNum: String, chemId: String) = {
    compareEntity.lstNum == lstNum && compareEntity.chemIds.contains(chemId)
  }


  printToFile(new File("good.txt")) { p =>
    (chemIdRight ::: lstRight).map(_.toString).foreach(p.println)
  }

  printToFile(new File("unknown.txt")) { p =>
    fCodeWrong.map(_.toString).foreach(p.println)
  }

  printToFile(new File("unknown2.txt")) { p =>
    lstWrong.map(_.toString).foreach(p.println)
  }

  fileToSearch.close()
  fullFile.close()
  secondFileToSearch.close()
}
