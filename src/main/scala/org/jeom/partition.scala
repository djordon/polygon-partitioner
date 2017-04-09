package org.jeom

import scala.collection.immutable.TreeSet
import scala.collection.JavaConversions._
import scala.collection.Searching.{search, Found, InsertionPoint, SearchResult}

import com.vividsolutions.jts.geom.{Polygon, Coordinate}
import com.vividsolutions.jts.index.strtree.SIRtree

import GeometryUtils.IterablePolygon


object OrthogonalPolygonPartitioner {

  def extractCorners(polygon: Polygon): List[Corner] = {
    val boundary: List[Coordinate] = polygon.toList.tail
    val extended: List[Coordinate] = Stream
      .continually(boundary)
      .flatten
      .take(boundary.length + 3)
      .toList

    extended
      .sliding(3, 1)
      .map(Corner.apply)
      .toList
  }

  private def sweepFolder(sweepVertically: Boolean)(
      tuple: Tuple2[TreeSet[Double], List[ExtendedCorner]],
      corner: Corner
    ): Tuple2[TreeSet[Double], List[ExtendedCorner]] = {

    val (tm, ecs) = tuple
    val z: Double = if (sweepVertically) corner.x else corner.y
    val doExtend: Boolean = sweepVertically == (corner.angle.abs != 90)

    if (tm.contains(z))
      (corner, doExtend) match {
        case (Corner(_, true, _), true) => (tm - z, corner.extend(tm - z) :: ecs)
        case _ => (tm - z, ecs)
      }
    else
      (corner, doExtend) match {
        case (Corner(_, true, _), true) => (tm + z, corner.extend(tm) :: ecs)
        case _ => (tm + z, ecs)
      }
  }

  private def cornerFolder(
      stacks: Tuple3[List[Point], List[Point], List[Point]],
      corner: CornerPoint): Tuple3[List[Point], List[Point], List[Point]] = {

    corner match {
      case ExtendedCorner(source, dest, 0) =>
        (stacks._1, source :: stacks._2, dest :: stacks._3)
      case ExtendedCorner(source, dest, 90) =>
        (dest :: stacks._1, stacks._2, source :: stacks._3)
      case ExtendedCorner(source, dest, 180) =>
        (dest :: stacks._1, dest :: stacks._2, stacks._3)
      case ExtendedCorner(source, dest, -90) =>
        (source :: stacks._1, dest :: stacks._2, dest :: stacks._3)
      case Corner(source, false, 90) =>
        (source :: stacks._1, stacks._2, stacks._3)
      case Corner(source, false, 180) =>
        (stacks._1, source :: stacks._2, stacks._3)
      case Corner(source, false, -90) =>
        (stacks._1, stacks._2, source :: stacks._3)
      case _ => stacks
    }
  }

  def makeRectangleCorners(corners: List[Corner]): List[CornerPoint] = {
    val startsVertically: Boolean = corners.head.angle.abs != 90

    val hc: List[Corner] = if (startsVertically) corners.tail else corners.init
    val vc: List[Corner] = if (startsVertically) corners.init else corners.tail

    val init: Tuple2[TreeSet[Double], List[ExtendedCorner]] = (TreeSet(), Nil)
    val vEdges: List[ExtendedCorner] = hc
      .sorted(CornerOrderingX)
      .foldLeft(init)(sweepFolder(false))
      ._2

    val hEdges: List[ExtendedCorner] = (vEdges.flatMap(_.toListCorner) ++ vc)
      .sorted(CornerOrderingY)
      .foldLeft(init)(sweepFolder(true))
      ._2

    vEdges ++ hEdges ++ corners.tail.filterNot(_.isConvex)
  }

  private def extractIndex(sr: SearchResult): Int = sr match { 
    case Found(i) => i + 1
    case InsertionPoint(i) => i
  }

  def extractRectangles(cornersPoints: List[CornerPoint]): List[Rectangle] = {
    val init: Tuple3[List[Point], List[Point], List[Point]] = (Nil, Nil, Nil)
  
    val (upperLeft, lowerLeft, lowerRight) = cornersPoints.foldLeft(init)(cornerFolder)
    val ul: Vector[Point] = upperLeft.toVector.sorted(PointOrderingX)
    val lr: Vector[Point] = lowerRight.toVector.sorted(PointOrderingY)
  
    lowerLeft map { p =>
      val i1 = extractIndex(ul.search(p)(PointOrderingX))
      val i2 = extractIndex(lr.search(p)(PointOrderingY))
      Rectangle(ul(i1), lr(i2))
    }
  }

  def partition: Polygon => List[Rectangle] =
    extractCorners _ andThen makeRectangleCorners _ andThen extractRectangles _
}
