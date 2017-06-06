package org.partitioner

import com.vividsolutions.jts.geom._


object GeometryUtils {
  val geometryFactory = new GeometryFactory()

  implicit class IterableCollection(val gc: GeometryCollection) extends Iterable[Geometry] {
    override def iterator: Iterator[Geometry] =
      (0 until gc.getNumGeometries).view.toIterator.map(gc.getGeometryN(_))
  }

  implicit class IterablePolygon(val pg: Polygon) extends Iterable[Coordinate] {
    override def iterator: Iterator[Coordinate] = pg.getExteriorRing.getCoordinates.toIterator

    def getHoles: List[Polygon] = {
      val holes: IndexedSeq[LinearRing] = for {i <- 0 until pg.getNumInteriorRing }
        yield pg.getInteriorRingN(i).asInstanceOf[LinearRing]

      holes.map(geometryFactory.createPolygon(_)).toList
    }
  }

  def emptyPolygon: Polygon = geometryFactory.createPolygon(Array[Coordinate]())
}
