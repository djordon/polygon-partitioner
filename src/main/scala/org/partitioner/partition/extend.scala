package org.partitioner.partition

import org.partitioner._

import scala.annotation.switch
import scala.collection.immutable.TreeSet


case class LineContainer(
    openedLines: TreeSet[Double] = TreeSet(),
    cornerLines: List[CornerLine] = Nil)


trait RectilinearLineSweeping {
  def lineAction(treeSet: TreeSet[Double])(cn: CornerGeometry): CornerLine

  def setActions(corners: List[CornerGeometry], opened: TreeSet[Double])(
    implicit extendVertically: Boolean): Map[String, List[CornerGeometry]]

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

  def adjustCornersGeometries(corners: List[CornerGeometry])(
    implicit extendVertically: Boolean): List[CornerLine] = {

    val lineContainer: LineContainer = corners
      .groupBy(_.z(!extendVertically))
      .toList
      .sortBy(_._1)
      .map(_._2)
      .foldLeft(LineContainer())(lineSweeper)

    lineContainer.cornerLines
  }
}


object OrthogonalPolygonCornerExtender {

  def extendCorner(treeSet: TreeSet[Double])(cn: Corner): CornerLine = {

    val destination: Point = (cn.angle: @switch) match {
      case 0 => Point((treeSet - cn.x).from(cn.x).firstKey, cn.y)
      case 180 => Point((treeSet - cn.x).to(cn.x).lastKey, cn.y)
      case -90 => Point(cn.x, (treeSet - cn.y).to(cn.y).lastKey)
      case 90 => Point(cn.x, (treeSet - cn.y).from(cn.y).firstKey)
    }

    CornerLine(cn.point, destination, cn.angle)
  }

  def setActions(corners: List[Corner], opened: TreeSet[Double])(
    implicit extendVertically: Boolean): Map[String, List[Corner]] = {

    val toOpenClose: Map[String, List[Corner]] = corners
      .groupBy(cn => cn.point)
      .filter(_._2.length == 1)
      .map(_._2.head)
      .toList
      .groupBy(cn => if (opened.contains(cn.z)) "toClose" else "toOpen")

    val toExtend: List[Corner] = corners filter { cn =>
      (extendVertically == (cn.angle.abs == 90)) && cn.isConcave
    }

    toOpenClose + ("toExtend" -> toExtend)
  }

  private def lineSweeper(container: LineContainer, corners: List[Corner])(
    implicit extendVertically: Boolean): LineContainer = {

    val actions = setActions(corners, container.openedLines)

    val opened: TreeSet[Double] = container.openedLines ++
      actions.getOrElse("toOpen", Nil).map(_.z)

    val closed: TreeSet[Double] = opened --
      actions.getOrElse("toClose", Nil).map(_.z)

    val extended: List[CornerLine] = actions
      .getOrElse("toExtend", Nil)
      .map(extendCorner(opened))

    LineContainer(closed, extended ::: container.cornerLines)
  }

  private def makeChord(pointMap: Map[Point, Corner])(ec: CornerLine): CornerGeometry = {
    pointMap.getOrElse(ec.dest, ec) match {
      case co: Corner => Chord(Corner(ec.source, true, ec.angle), co)
      case ex: CornerLine => ex
    }
  }

  private def uniqueExtendedCorners(set: Set[CornerLine], ec: CornerLine)
      : Set[CornerLine] = {
    if (set contains ec.swap) set else set + ec
  }

  def extendCorners(corners: List[Corner])(
    implicit extendVertically: Boolean): List[CornerGeometry] = {

    val concavePointMap: Map[Point, Corner] = corners
      .filter(_.isConcave)
      .map(cn => (cn.point, cn))
      .toMap

    val lineContainer: LineContainer = corners
      .groupBy(_.z(!extendVertically))
      .toList
      .sortBy(_._1)
      .map(_._2)
      .foldLeft(LineContainer())(lineSweeper)

    lineContainer
      .cornerLines
      .foldLeft(Set[CornerLine]())(uniqueExtendedCorners)
      .toList
      .map { makeChord(concavePointMap)(_) }
  }
}


object CornerLineAdjuster {

  def extendCorner(treeSet: TreeSet[Double])(cn: Corner): CornerLine = {
    val destination: Point = (cn.angle: @switch) match {
      case 0 => Point(treeSet.from(cn.x).firstKey, cn.y)
      case 180 => Point(treeSet.to(cn.x).lastKey, cn.y)
      case -90 => Point(cn.x, treeSet.to(cn.y).lastKey)
      case 90 => Point(cn.x, treeSet.from(cn.y).firstKey)
    }

    CornerLine(cn.point, destination, cn.angle)
  }

  def setActions(corners: List[CornerGeometry], opened: TreeSet[Double])(
      implicit extendVertically: Boolean): Map[String, List[Corner]] = {

    val destinationCorners: List[Corner] = corners
      .collect { case cn: CornerLine => cn.toListCorner.last }

    val toOpenClose: Map[String, List[Corner]] = corners
      .collect { case cn: Corner => cn }
      .groupBy { cn => if (opened.contains(cn.z)) "toClose" else "toOpen" }

    val toOpen: List[Corner] = destinationCorners ::: toOpenClose
      .getOrElse("toOpen", Nil)

    val toClose: List[Corner] = destinationCorners ::: toOpenClose
      .getOrElse("toClose", Nil)

    val toExtend: List[Corner] = corners
      .collect { case cn: CornerLine => cn.toListCorner.head }

    Map("toOpen" -> toOpen, "toClose" -> toClose, "toExtend" -> toExtend)
  }

  private def lineSweeper(container: LineContainer, corners: List[CornerGeometry])(
      implicit extendVertically: Boolean): LineContainer = {

    val actions = setActions(corners, container.openedLines)

    val opened: TreeSet[Double] = container.openedLines ++
      actions.getOrElse("toOpen", Nil).map(_.z)

    val closed: TreeSet[Double] = opened --
      actions.getOrElse("toClose", Nil).map(_.z)

    val extended: List[CornerLine] = actions
      .getOrElse("toExtend", Nil)
      .map(extendCorner(opened))

    LineContainer(closed, extended ::: container.cornerLines)
  }

  def adjustCornersLines(corners: List[CornerGeometry])(
      implicit extendVertically: Boolean): List[CornerLine] = {

    val lineContainer: LineContainer = corners
      .groupBy(_.z(!extendVertically))
      .toList
      .sortBy(_._1)
      .map(_._2)
      .foldLeft(LineContainer())(lineSweeper)

    lineContainer.cornerLines
  }
}
