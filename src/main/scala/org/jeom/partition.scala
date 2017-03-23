package org.jeom
// import scala.collection.JavaConversions._
// import scala.collection.JavaConverters._

import com.vividsolutions.jts.algorithm.Angle
import com.vividsolutions.jts.geom.{Polygon, Envelope, Coordinate}
import com.vividsolutions.jts.index.strtree.STRtree

import GeometryUtils.IterablePolygon


case class Corner(coord: Coordinate, isConvex: Boolean, direction: Int) {
  def extend: Envelope = {
    val p0: Coordinate = direction match {
      case 0 => new Coordinate(Double.PositiveInfinity, coord.y)
      case 90 => new Coordinate(coord.x, Double.PositiveInfinity)
      case 180 => new Coordinate(Double.NegativeInfinity, coord.y)
      case -90 => new Coordinate(coord.x, Double.NegativeInfinity)
      case _ => new Coordinate()
    }
  
    new Envelope(coord, p0)
  }
}


object Corner {
  def isConvexCorner(corner: List[Coordinate]): Boolean =
    Angle.angleBetweenOriented(corner(0), corner(1), corner(2)) < 0

  def edgeDirection(vec: List[Coordinate]): Int = 
    Angle.toDegrees(Angle.angle(vec.head, vec.last)).toInt

  def apply(coords: List[Coordinate]) =
    new Corner(coords(1), isConvexCorner(coords), edgeDirection(coords.take(2)))
}

object Whatever {
  def getCorners(polygon: Polygon): Iterator[Corner] = {
    val boundary: List[Coordinate] = polygon.toList.tail
    val extended: List[Coordinate] = Stream
      .continually(boundary)
      .flatten
      .take(boundary.length + 2)
      .toList

    extended.sliding(3, 1).map(Corner.apply)
  }
}
