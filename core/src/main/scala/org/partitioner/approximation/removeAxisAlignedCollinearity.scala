package org.partitioner

import com.vividsolutions.jts.geom.{Coordinate, LinearRing, Polygon}
import GeometryUtils.{IterablePolygon, geometryFactory}


/**
 * Returns a topologically equivalent polygon where axis aligned line
 * segments are as long as possible.
 *
 * A polygon can contain superfluous points along the boundary, where
 * removing such points does not change the shape. This method removes
 * instances where these superfluous points occur on axis aligned line
 * segments.
 *
 * The input polygon is not modified.
 *
 * An example of this is where the input is a rectangle with 5 points
 * (where a point was added at the midpoint of the top edge). This
 * function would return the same rectangle, but with 4 points.
 *
 * @param polygon the input polygon
 *
 * @return an modified version of the input polygon.
 */
object removeAxisAlignedCollinearity extends Function1[Polygon, Polygon] {

  private[this] def filterVertices(pg: Polygon): List[Coordinate] = {
    val vertices: List[Coordinate] = pg.toList ::: pg.toList
    val reduced: List[Coordinate] = vertices
      .drop(2)
      .foldLeft(vertices.take(2))(rectilinearFolder)

    val boundary: List[Coordinate] = reduced.take(reduced.length / 2 + 1).tail
    val last: Coordinate = boundary.last

    if (last == boundary.head) boundary else last :: boundary
  }

  @inline
  private[this] def isAxisAligned(co: List[Coordinate]): Boolean = (co: @unchecked) match {
    case a :: b :: c :: Nil => (a.x == c.x && b.x == c.x) || (a.y == c.y && b.y == c.y)
  }

  private[this] def rectilinearFolder(a: List[Coordinate], b: Coordinate): List[Coordinate] = {
    if (isAxisAligned { b :: a.take(2) }) b :: a.tail else b :: a
  }

  private[this] def vertices2LinearRing(vertices: List[Coordinate]): LinearRing = {
    geometryFactory.createLinearRing(vertices.toArray)
  }

  private[this] def removeAxisAlignedCollinearitySimple: Polygon => LinearRing = {
    filterVertices _ andThen vertices2LinearRing _
  }

  def apply(polygon: Polygon): Polygon = {
    val shell: LinearRing = removeAxisAlignedCollinearitySimple(polygon)
    val holes: List[LinearRing] = polygon
      .getHoles
      .map(removeAxisAlignedCollinearitySimple)

    val pg: Polygon = geometryFactory.createPolygon(shell, holes.toArray)
    pg.normalize()
    pg
  }
}
