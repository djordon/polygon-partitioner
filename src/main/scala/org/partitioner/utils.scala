package org.partitioner

import com.vividsolutions.jts.geom.{Coordinate, Geometry, GeometryCollection, GeometryFactory, Polygon}


object GeometryUtils {
  val geometryFactory = new GeometryFactory()

  implicit class IterableCollection(val gc: GeometryCollection) extends Iterable[Geometry] {
    override def iterator: Iterator[Geometry] =
      (0 until gc.getNumGeometries).view.toIterator.map(gc.getGeometryN(_))
  }

  implicit class IterablePolygon(val pg: Polygon) extends Iterable[Coordinate] {
    override def iterator: Iterator[Coordinate] = pg.getExteriorRing.getCoordinates.toIterator

    def getHoles: Seq[List[Coordinate]] = {
      val holes = for { i <- 0 until pg.getNumInteriorRing }
        yield pg.getInteriorRingN(i).getCoordinates.toList

      holes.toSeq
    }
  }

  def emptyPolygon: Polygon = geometryFactory.createPolygon(Array[Coordinate]())
}
