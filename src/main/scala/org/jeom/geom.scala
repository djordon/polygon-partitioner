package org.jeom

import scala.collection.JavaConversions._

import com.vividsolutions.jts.algorithm.Angle
import com.vividsolutions.jts.geom.Coordinate
import com.vividsolutions.jts.index.strtree.SIRtree


case class Point(x: Double, y: Double) {
  def toTuple: Tuple2[Double, Double] = (x, y)
}


object Point {
  def apply(coord: Coordinate) = new Point(coord.x, coord.y)
}

object XOrdering extends Ordering[Point] {
    def compare(a: Point, b: Point) =
      implicitly[Ordering[Tuple2[Double, Double]]].compare((a.x, a.y), (b.x, b.y))
}


object YOrdering extends Ordering[Point] {
    def compare(a: Point, b: Point) =
      implicitly[Ordering[Tuple2[Double, Double]]].compare((a.y, a.x), (b.y, b.x))
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

  def extendsVertically: Boolean = angle.abs == 90

  def extendCorner(edgeTree: SIRtree): ExtendedCorner = {
    val candidates: Iterable[Double] = edgeTree
      .query(this.extensionCoord)
      .toList.asInstanceOf[List[Double]]
      .filter(this.extensionOrder(_))

    val z: Double = if (angle == 90 || angle == 0) candidates.min else candidates.max
    val destination: Point = if (angle.abs == 90) Point(point.x, z) else Point(z, point.y)

    ExtendedCorner(point, destination, angle)
  }

  def extensionCoord: Double = if (angle.abs == 90) point.x else point.y

  def extensionOrder(xy: Double): Boolean = angle match {
      case 0 => xy > point.x
      case 90 => xy > point.y
      case 180 => xy < point.x
      case -90 => xy < point.y
  }
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
