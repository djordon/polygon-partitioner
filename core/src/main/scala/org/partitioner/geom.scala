package org.partitioner

import com.vividsolutions.jts.algorithm.{Angle => AngleJTS}
import com.vividsolutions.jts.geom.{Coordinate, Polygon}


/**
 * A generic point on the plane
 *
 * @param x
 * @param y
 */
case class Point(x: Double, y: Double)


object Point {
  def apply(coord: Coordinate) = new Point(coord.x, coord.y)
}


object PointOrderingX extends Ordering[Point] {
    def compare(a: Point, b: Point) =
      implicitly[Ordering[(Double, Double)]].compare((a.x, a.y), (b.x, b.y))
}


object PointOrderingY extends Ordering[Point] {
    def compare(a: Point, b: Point) =
      implicitly[Ordering[(Double, Double)]].compare((a.y, a.x), (b.y, b.x))
}


object CornerOrderingX extends Ordering[CornerGeometry] {
    def compare(a: CornerGeometry, b: CornerGeometry) =
      implicitly[Ordering[(Double, Double)]].compare((a.x, a.y), (b.x, b.y))
}


object CornerOrderingY extends Ordering[CornerGeometry] {
    def compare(a: CornerGeometry, b: CornerGeometry) =
      implicitly[Ordering[(Double, Double)]].compare((a.y, a.x), (b.y, b.x))
}


case class Rectangle(upperLeft: Point, lowerRight: Point) {
  def upperRight: Point = Point(lowerRight.x, upperLeft.y)
  def lowerLeft: Point = Point(upperLeft.x, lowerRight.y)
  def toPolygon: Polygon = GeometryUtils.geometryFactory.createPolygon(
    List(lowerLeft, upperLeft, upperRight, lowerRight, lowerLeft)
      .map(p => new Coordinate(p.x, p.y)).toArray
  )
}


trait CornerGeometry {
  val pointsVertically: Boolean = angle.abs == 90
  def angle: Int
  def isConcave: Boolean
  def oppositeAngle: Int = ((angle + 270) % 360) - 90
  def point: Point
  def toListCorner: List[CornerGeometry]
  def w: Double = if (pointsVertically) y else x
  def x: Double = point.x
  def y: Double = point.y
  def z(implicit vertical: Boolean): Double = if (vertical) y else x
}


/**
 * Class that represents a vertex of an orthogonal polygon
 *
 * @param point
 * @param isConcave
 * @param angle
 */
case class Corner(point: Point, isConcave: Boolean, angle: Int) extends CornerGeometry {
  def toListCorner: List[Corner] = List(this)
}


object Corner {
  def apply(coordinates: List[Coordinate]) = new Corner(
    point = Point.apply(coordinates(1)),
    isConcave = isConcaveCorner(coordinates),
    angle = edgeDirection(coordinates.init)
  )

  def edgeDirection: PartialFunction[List[Coordinate], Int] = {
    case a :: b :: Nil => AngleJTS.toDegrees(AngleJTS.angle(a, b)).toInt
  }

  def isConcaveCorner: PartialFunction[List[Coordinate], Boolean] = {
    case a :: b :: c :: Nil => AngleJTS.angleBetweenOriented(a, b, c) < 0
  }
}

/**
 * A class that represents an interior line of a polygon
 *
 * @param source
 * @param dest
 * @param angle
 */
case class CornerLine(source: Point, dest: Point, angle: Int) extends CornerGeometry {
  def isConcave: Boolean = true
  def point: Point = source
  def toListCorner: List[Corner] = {
    List(Corner(source, true, angle), Corner(dest, false, oppositeAngle))
  }
}

/**
 * A class that represents a chord of an orthogonal polygon
 *
 * @param source
 * @param dest
 */
case class Chord(source: Corner, dest: Corner) extends CornerGeometry {
  lazy val left: Corner = if (source.x < dest.x) source else dest

  def angle: Int = source.angle
  def isConcave: Boolean = true
  def point: Point = source.point
  def swap: Chord = Chord(dest, source)
  def toCornerLine: CornerLine = CornerLine(source.point, dest.point, source.angle)
  def toListCorner: List[Corner] = List(source, dest)
  def xMax: Double = if (source.x > dest.x) source.x else dest.x
  def xMin: Double = if (source.x < dest.x) source.x else dest.x
  def yMax: Double = if (source.y > dest.y) source.y else dest.y
  def yMin: Double = if (source.y < dest.y) source.y else dest.y
}
