package org.jeom

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

  private def buildIntervalTree(corners: List[CornerPoint], isVertical: Boolean)
    : SIRtree = {

    val tree: SIRtree = new SIRtree()
    val edges: Iterator[List[CornerPoint]] = corners.grouped(2)

    if (isVertical) 
      edges foreach { e => tree.insert(e.head.y, e.last.y, e.head.x) }
    else
      edges foreach { e => tree.insert(e.head.x, e.last.x, e.head.y) }

    tree
  }

  private def folder(
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

  private def extendCorners(
      corners: List[Corner],
      edges: List[Corner],
      isVertical: Boolean): List[ExtendedCorner] = {

    val edgeTree: SIRtree = buildIntervalTree(edges, !isVertical)

    corners
      .filter(c => c.isConvex && isVertical == c.extendsVertically)
      .map(_.extendCorner(edgeTree))
  }
  
  def makeRectangleCorners(corners: List[Corner]): List[CornerPoint] = {
    val startsVertically: Boolean = corners.head.angle.abs != 90

    val hc: List[Corner] = if (startsVertically) corners.tail else corners.init
    val vc: List[Corner] = if (startsVertically) corners.init else corners.tail

    val vEdges: List[ExtendedCorner] = extendCorners(corners.tail, hc, true)
    val vCorners: List[Corner] = vEdges.flatMap(_.toListCorner)

    val hEdges: List[ExtendedCorner] = extendCorners(corners.tail, vCorners ++ vc, false)

    vEdges ++ hEdges ++ corners.tail.filterNot(_.isConvex)
  }

  private def extractIndex(sr: SearchResult): Int = sr match { 
    case Found(i) => i + 1
    case InsertionPoint(i) => i
  }

  def extractRectangles(cornersPoints: List[CornerPoint]): List[Rectangle] = {
    val init: Tuple3[List[Point], List[Point], List[Point]] = (Nil, Nil, Nil)
  
    val (upperLeft, lowerLeft, lowerRight) = cornersPoints.foldLeft(init)(folder)
    val ul: Vector[Point] = upperLeft.toVector.sorted(XOrdering)
    val lr: Vector[Point] = lowerRight.toVector.sorted(YOrdering)
  
    lowerLeft map { p =>
      val i1 = extractIndex(ul.search(p)(XOrdering))
      val i2 = extractIndex(lr.search(p)(YOrdering))
      Rectangle(ul(i1), lr(i2))
    }
  }

  def partition: Polygon => List[Rectangle] =
    extractCorners _ andThen makeRectangleCorners _ andThen extractRectangles _
}
