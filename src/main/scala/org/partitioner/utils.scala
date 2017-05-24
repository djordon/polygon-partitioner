package org.partitioner

import com.vividsolutions.jts.geom.{GeometryCollection, GeometryFactory, Geometry, Polygon, Coordinate}


object GeometryUtils {
  val geometryFactory = new GeometryFactory()

  implicit class IterableCollection(val gc: GeometryCollection) extends Iterable[Geometry] {
    override def iterator: Iterator[Geometry] =
      (0 until gc.getNumGeometries).view.toIterator.map(gc.getGeometryN(_))
  }

  implicit class IterablePolygon(val pg: Polygon) extends Iterable[Coordinate] {
    override def iterator: Iterator[Coordinate] = pg.getCoordinates.toIterator
  }

  def emptyPolygon: Polygon = (new GeometryFactory()).createPolygon(Array[Coordinate]())
}
