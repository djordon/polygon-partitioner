package org.jeom
// import scala.collection.JavaConversions._
// import scala.collection.JavaConverters._

import com.vividsolutions.jts.algorithm.Angle
import com.vividsolutions.jts.geom.{Polygon, LineString, Coordinate}
import com.vividsolutions.jts.index.strtree.STRtree

import GeometryUtils.IterablePolygon


case class Corner(coord: Coordinate, isConvex: Boolean, headAngle: Int, tailAngle: Int) {

  def extend(sup: Double): LineString = {
    val p0: Coordinate = angle match {
      case 0 => new Coordinate(sup, coord.y)
      case 90 => new Coordinate(coord.x, sup)
      case 180 => new Coordinate(sup, coord.y)
      case -90 => new Coordinate(coord.x, sup)
      case _ => new Coordinate()
    }
  
    GeometryUtils.geometryFactory.createLineString(Array(coord, p0))
  }

  val angle = headAngle

  def predicate(xy: Double): Boolean = angle match {
      case 0 => xy > coord.x
      case 90 => xy > coord.y
      case 180 => xy < coord.x
      case -90 => xy < coord.y
  }

  def renameMe: Double = if (angle.abs == 90) coord.x else coord.y
}


object Corner {
  def apply(coords: List[Coordinate]) = new Corner(
    coord = coords(1), 
    isConvex = isConvexCorner(coords), 
    headAngle = edgeDirection(coords.init),
    tailAngle = edgeDirection(coords.tail)
  )

  def edgeDirection(vec: List[Coordinate]): Int = 
    Angle.toDegrees(Angle.angle(vec.head, vec.last)).toInt

  def isConvexCorner(corner: List[Coordinate]): Boolean =
    Angle.angleBetweenOriented(corner(0), corner(1), corner(2)) < 0
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
