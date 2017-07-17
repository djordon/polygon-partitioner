package org.partitioner.orthogonal

import com.vividsolutions.jts.geom.{Coordinate, Polygon}
import org.partitioner.{Corner, GeometryUtils}
import GeometryUtils.IterablePolygon


object makeCorners extends Function1[List[Coordinate], List[Corner]] {

  def apply(boundary: List[Coordinate]): List[Corner] = {
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
}


object extractCorners extends Function1[Polygon, List[List[Corner]]] {

  def apply(polygon: Polygon): List[List[Corner]] = {
    val boundary: List[Coordinate] = polygon.toList.tail
    val holes: List[List[Coordinate]] = polygon.getHolesCoordinates.map(_.tail)

    (boundary :: holes).map(makeCorners)
  }
}