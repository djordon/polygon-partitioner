package org.jeom
import scala.collection.JavaConversions._
import scala.collection.Searching.{search, Found, InsertionPoint, SearchResult}

import com.vividsolutions.jts.algorithm.Angle
import com.vividsolutions.jts.geom.{Polygon, LineString, Coordinate}
import com.vividsolutions.jts.index.strtree.SIRtree

import GeometryUtils.IterablePolygon


case class Point(x: Double, y: Double) {
  def toTuple: Tuple2[Double, Double] = (x, y)
}

object Point {
  def apply(coord: Coordinate) = new Point(coord.x, coord.y)
}

case class Rectangle(upperLeft: Point, lowerRight: Point)

trait CornerPoint {
  def isConvex: Boolean
  def point: Point
  def angle: Int
  def x: Double = point.x
  def y: Double = point.y
}


case class ExtendedCorner(source: Point, dest: Point, angle: Int) extends CornerPoint {
  def toListCorner: List[Corner] = List(Corner(source, false, 0), Corner(dest, false, 0))
  def point: Point = source
  def isConvex: Boolean = true
}


case class Corner(point: Point, isConvex: Boolean, angle: Int) extends CornerPoint {

  def extendsVertically: Boolean = angle == 90 || angle == -90

  def extendCorner(edgeTree: SIRtree): ExtendedCorner = {
    val candidates: Iterable[Double] = edgeTree
      .query(this.renameMe)
      .toList.asInstanceOf[List[Double]]
      .filter(this.predicate(_))

    val z: Double = if (angle == 90 || angle == 0) candidates.min else candidates.max
    val destination: Point = angle match {
      case 0 => new Point(z, point.y)
      case 90 => new Point(point.x, z)
      case 180 => new Point(z, point.y)
      case -90 => new Point(point.x, z)
      case _ => new Point(0, 0)
    }

    ExtendedCorner(point, destination, angle)
  }

  def predicate(xy: Double): Boolean = angle match {
      case 0 => xy > point.x
      case 90 => xy > point.y
      case 180 => xy < point.x
      case -90 => xy < point.y
  }

  def renameMe: Double = if (angle.abs == 90) point.x else point.y
}


object Corner {
  def apply(coords: List[Coordinate]) = new Corner(
    point = Point.apply(coords(1)),
    isConvex = isConvexCorner(coords),
    angle = edgeDirection(coords.init)
  )

  def edgeDirection(vec: List[Coordinate]): Int = 
    Angle.toDegrees(Angle.angle(vec.head, vec.last)).toInt

  def isConvexCorner(corner: List[Coordinate]): Boolean =
    Angle.angleBetweenOriented(corner(0), corner(1), corner(2)) < 0
}


case class CornerStack(upperLeft: List[Point], lowerLeft: List[Point], lowerRight: List[Point]) {
  def prependUpperLeft(point: Point): CornerStack = copy(upperLeft = point :: upperLeft)
  def prependLowerLeft(point: Point): CornerStack = copy(lowerLeft = point :: lowerLeft)
  def prependLowerRight(point: Point): CornerStack = copy(lowerRight = point :: lowerRight)
}


object OrthogonalPolygonPartitioner {
  def extractCorners(polygon: Polygon): List[Corner] = {
    val boundary: List[Coordinate] = polygon.toList.tail
    val extended: List[Coordinate] = Stream
      .continually(boundary)
      .flatten
      .take(boundary.length + 2)
      .toList

    extended
      .sliding(3, 1)
      .map(Corner.apply)
      .toList
  }

  def buildIntervalTree(corners: List[CornerPoint]): SIRtree = {
    val tree: SIRtree = new SIRtree()
    val isVertical: Boolean = corners.head.angle.abs != 90
    val edges: Iterator[List[CornerPoint]] = corners.grouped(2)

    if (isVertical) 
      edges foreach { e => tree.insert(e.head.y, e.last.y, e.head.x) }
    else
      edges foreach { e => tree.insert(e.head.x, e.last.x, e.head.y) }

    tree
  }

  def folder(stacks: Tuple3[List[Point], List[Point], List[Point]], corner: CornerPoint): 
      Tuple3[List[Point], List[Point], List[Point]] = {
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
    val boundary: List[Corner] = corners.last :: corners
    val isVertical: Boolean = boundary.head.angle.abs != 90
    val horizontal: List[Corner] = if (isVertical) boundary.tail else boundary.init
    val vertical: List[Corner] = if (isVertical) boundary.init else boundary.tail

    val horizontalEdgeTree: SIRtree = buildIntervalTree(horizontal)
    val verticalEdges: List[ExtendedCorner] = (corners
      .filter(_.isConvex)
      .filter(_.extendsVertically)
      .map(_.extendCorner(horizontalEdgeTree)))

    val vs: List[Corner] = verticalEdges.flatMap(_.toListCorner)
    val verticalEdgeTree: SIRtree = buildIntervalTree(vertical ++ vs)

    val horizontalEdges: List[ExtendedCorner] = corners
      .filter(_.isConvex)
      .filter(!_.extendsVertically)
      .map(_.extendCorner(verticalEdgeTree))

    verticalEdges ++ horizontalEdges ++ corners.filterNot(_.isConvex)
  }

  object XOrdering extends Ordering[Point] {
      def compare(a: Point, b: Point) =
        implicitly[Ordering[Tuple2[Double, Double]]].compare((a.x, a.y), (b.x, b.y))
  }

  object YOrdering extends Ordering[Point] {
      def compare(a: Point, b: Point) =
        implicitly[Ordering[Tuple2[Double, Double]]].compare((a.y, a.x), (b.y, b.x))
  }

  def extractIndex(sr: SearchResult): Int = sr match { 
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
