package org.partitioner

import com.vividsolutions.jts.algorithm.Angle
import com.vividsolutions.jts.geom.{Coordinate, LineString}


case class Point(x: Double, y: Double) {
  def toTuple: Tuple2[Double, Double] = (x, y)
}


object Point {
  def apply(coord: Coordinate) = new Point(coord.x, coord.y)
}


object PointOrderingX extends Ordering[Point] {
    def compare(a: Point, b: Point) =
      implicitly[Ordering[Tuple2[Double, Double]]].compare((a.x, a.y), (b.x, b.y))
}


object PointOrderingY extends Ordering[Point] {
    def compare(a: Point, b: Point) =
      implicitly[Ordering[Tuple2[Double, Double]]].compare((a.y, a.x), (b.y, b.x))
}


object CornerOrderingX extends Ordering[CornerPoint] {
    def compare(a: CornerPoint, b: CornerPoint) =
      implicitly[Ordering[Tuple2[Double, Double]]].compare((a.x, a.y), (b.x, b.y))
}


object CornerOrderingY extends Ordering[CornerPoint] {
    def compare(a: CornerPoint, b: CornerPoint) =
      implicitly[Ordering[Tuple2[Double, Double]]].compare((a.y, a.x), (b.y, b.x))
}


case class Rectangle(upperLeft: Point, lowerRight: Point) {
  def upperRight: Point = Point(lowerRight.x, upperLeft.y)
  def lowerLeft: Point = Point(upperLeft.x, lowerRight.y)
}


trait CornerPoint {
  def isConcave: Boolean
  def point: Point
  def angle: Int
  def x: Double = point.x
  def y: Double = point.y
  def toListCorner: List[CornerPoint]
}


case class Corner(point: Point, isConcave: Boolean, angle: Int) extends CornerPoint {
  def z(implicit extendVertically: Boolean): Double = if (extendVertically) y else x
  def toListCorner: List[Corner] = List(this)
}


object Corner {
  def apply(coords: List[Coordinate]) = new Corner(
    point = Point.apply(coords(1)),
    isConcave = isConcaveCorner(coords),
    angle = edgeDirection(coords.init)
  )

  def edgeDirection(vec: List[Coordinate]): Int = 
    Angle.toDegrees(Angle.angle(vec.head, vec.last)).toInt

  def isConcaveCorner(corner: List[Coordinate]): Boolean =
    Angle.angleBetweenOriented(corner(0), corner(1), corner(2)) < 0
}


case class ExtendedCorner(source: Point, dest: Point, angle: Int) extends CornerPoint {
  lazy val oppositeAngle = ((angle + 270) % 360) - 90

  def isConcave: Boolean = true
  def point: Point = source
  def swap: ExtendedCorner = ExtendedCorner(dest, source, oppositeAngle)
  def toListCorner: List[Corner] = {
    List(Corner(source, true, angle), Corner(dest, false, oppositeAngle))
  }
  def toLineString: LineString = {
    GeometryUtils.geometryFactory.createLineString(
      Array(new Coordinate(source.x, source.y),
        new Coordinate(dest.x, dest.y))
    )
  }
}


case class Chord(source: Corner, dest: Corner) extends CornerPoint {
  lazy val left: Corner = if (source.x < dest.x) source else dest
  def top: Double = if (source.y > dest.y) source.y else dest.y
  def bot: Double = if (source.y < dest.y) source.y else dest.y
  def isConcave: Boolean = true
  def point: Point = source.point
  def angle: Int = source.angle
  def toListCorner: List[Corner] = List(source, dest)
  def toLineString: LineString = {
    GeometryUtils.geometryFactory.createLineString(
      Array(new Coordinate(source.x, source.y),
        new Coordinate(dest.x, dest.y))
    )
  }
}

case class Vec(coord: Coordinate, angle: Double)


object Vec {
  def apply(a: List[Coordinate]) = 
    new Vec(a(1), Angle.toDegrees(Angle.angle(a.head, a.tail.head)))
}
