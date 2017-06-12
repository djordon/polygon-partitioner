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


object CornerOrderingX extends Ordering[CornerGeometry] {
    def compare(a: CornerGeometry, b: CornerGeometry) =
      implicitly[Ordering[Tuple2[Double, Double]]].compare((a.x, a.y), (b.x, b.y))
}


object CornerOrderingY extends Ordering[CornerGeometry] {
    def compare(a: CornerGeometry, b: CornerGeometry) =
      implicitly[Ordering[Tuple2[Double, Double]]].compare((a.y, a.x), (b.y, b.x))
}


case class Rectangle(upperLeft: Point, lowerRight: Point) {
  def upperRight: Point = Point(lowerRight.x, upperLeft.y)
  def lowerLeft: Point = Point(upperLeft.x, lowerRight.y)
}


trait CornerGeometry {
  def isConcave: Boolean
  def point: Point
  def angle: Int
  def x: Double = point.x
  def y: Double = point.y
  def toListCorner: List[CornerGeometry]
}


case class Corner(point: Point, isConcave: Boolean, angle: Int) extends CornerGeometry {
  def z(implicit extendVertically: Boolean): Double = if (extendVertically) y else x
  def toListCorner: List[Corner] = List(this)
}


object Corner {
  def apply(coordinates: List[Coordinate]) = new Corner(
    point = Point.apply(coordinates(1)),
    isConcave = isConcaveCorner(coordinates),
    angle = edgeDirection(coordinates.init)
  )

  def edgeDirection(vec: List[Coordinate]): Int = 
    Angle.toDegrees(Angle.angle(vec.head, vec.last)).toInt

  def isConcaveCorner(corner: List[Coordinate]): Boolean =
    Angle.angleBetweenOriented(corner(0), corner(1), corner(2)) < 0
}


case class CornerLine(source: Point, dest: Point, angle: Int) extends CornerGeometry {
  lazy val oppositeAngle = ((angle + 270) % 360) - 90

  def isConcave: Boolean = true
  def point: Point = source
  def swap: CornerLine = CornerLine(dest, source, oppositeAngle)
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


case class Chord(source: Corner, dest: Corner) extends CornerGeometry {
  lazy val left: Corner = if (source.x < dest.x) source else dest
  def yMax: Double = if (source.y > dest.y) source.y else dest.y
  def yMin: Double = if (source.y < dest.y) source.y else dest.y
  def xMax: Double = if (source.x > dest.x) source.x else dest.x
  def xMin: Double = if (source.x < dest.x) source.x else dest.x
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
