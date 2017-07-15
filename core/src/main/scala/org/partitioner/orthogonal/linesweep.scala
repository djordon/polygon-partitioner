package org.partitioner.orthogonal

import org.partitioner._

import scala.annotation.switch
import scala.collection.immutable.TreeSet


case class LineContainer(
    openedLines: TreeSet[Double] = TreeSet(),
    cornerLines: List[CornerLine] = Nil)


trait RectilinearLineSweeper {
  def setActions(corners: List[CornerGeometry], opened: TreeSet[Double])(
    implicit vertical: Boolean): Map[String, List[CornerGeometry]]

  def lineAction(treeSet: TreeSet[Double])(cn: CornerGeometry): CornerLine = {
    val destination: Point = (cn.angle: @switch) match {
      case 0 => Point(treeSet.from(cn.x).firstKey, cn.y)
      case 180 => Point(treeSet.to(cn.x).lastKey, cn.y)
      case -90 => Point(cn.x, treeSet.to(cn.y).lastKey)
      case 90 => Point(cn.x, treeSet.from(cn.y).firstKey)
    }

    CornerLine(cn.point, destination, cn.angle)
  }

  def lineSweeper(lines: LineContainer, corners: List[CornerGeometry])(
      implicit vertical: Boolean): LineContainer = {

    val actions = setActions(corners, lines.openedLines)

    val opened: TreeSet[Double] = lines.openedLines ++
      actions.getOrElse("toOpen", Nil).map(_.z)

    val closed: TreeSet[Double] = opened --
      actions.getOrElse("toClose", Nil).map(_.z)

    val adjusted: List[CornerLine] = actions
        .getOrElse("toAdjust", Nil).map(lineAction(opened))

    LineContainer(closed, adjusted ::: lines.cornerLines)
  }

  def createLineSweepGroups(corners: List[CornerGeometry])(
    implicit vertical: Boolean): List[List[CornerGeometry]] = {

    corners
      .groupBy(_.z(!vertical))
      .toList
      .sortBy(_._1)
      .map(_._2)
    }

  def adjustCornerGeometries(corners: List[CornerGeometry])(
    implicit vertical: Boolean): List[CornerLine] = {

    createLineSweepGroups(corners)
      .foldLeft(LineContainer())(lineSweeper)
      .cornerLines
  }
}


object OrthogonalPolygonCornerExtender extends RectilinearLineSweeper {

  override def lineAction(treeSet: TreeSet[Double])(cn: CornerGeometry): CornerLine = {
    super.lineAction(treeSet - cn.w)(cn)
  }

  def setActions(corners: List[CornerGeometry], opened: TreeSet[Double])(
    implicit extendVertically: Boolean): Map[String, List[CornerGeometry]] = {

    val toOpenClose: Map[String, List[CornerGeometry]] = corners
      .groupBy(cn => cn.point)
      .filter(_._2.length == 1)
      .map(_._2.head)
      .toList
      .groupBy(cn => if (opened.contains(cn.z)) "toClose" else "toOpen")

    val toAdjust: List[CornerGeometry] = corners filter { cn =>
      (extendVertically == cn.pointsVertically) && cn.isConcave
    }

    toOpenClose + ("toAdjust" -> toAdjust)
  }

  private def makeChord(pointMap: Map[Point, Corner])(ec: CornerLine): CornerGeometry = {
    pointMap.getOrElse(ec.dest, ec) match {
      case co: Corner => Chord(Corner(ec.source, true, ec.angle), co)
      case ex: CornerLine => ex
    }
  }

  def extendCorners(corners: List[Corner])(
    implicit extendVertically: Boolean): List[CornerGeometry] = {

    val concavePointMap: Map[Point, Corner] = corners
      .filter(_.isConcave)
      .map(cn => (cn.point, cn))
      .toMap

    adjustCornerGeometries(corners).toList.map { makeChord(concavePointMap)(_) }
  }
}


object CornerLineAdjuster extends RectilinearLineSweeper {

  def setActions(corners: List[CornerGeometry], opened: TreeSet[Double])(
    implicit vertical: Boolean): Map[String, List[Corner]] = {

    val toOpenClose: Map[String, List[Corner]] = corners
      .collect { case cn: Corner => cn }
      .groupBy { cn => if (opened.contains(cn.z)) "toClose" else "toOpen" }

    val toOpenPoints: Set[Point] = toOpenClose
      .getOrElse("toOpen", Nil)
      .map(_.point)
      .toSet

    val destinationCorners: List[Corner] = corners
      .collect { case cn: CornerLine => cn.toListCorner.last }
      .filterNot { cn => toOpenPoints.contains(cn.point) }

    val toOpen: List[Corner] = destinationCorners ::: toOpenClose
      .getOrElse("toOpen", Nil)

    val toClose: List[Corner] = destinationCorners ::: toOpenClose
      .getOrElse("toClose", Nil)

    val toAdjust: List[Corner] = corners
      .collect { case cn: CornerLine => cn.toListCorner.head }

    Map("toOpen" -> toOpen, "toClose" -> toClose, "toAdjust" -> toAdjust)
  }
}
