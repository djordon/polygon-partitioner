package org.partitioner

import com.vividsolutions.jts.geom._


object GeometryUtils {
  val geometryFactory = new GeometryFactory()

  implicit class IterablePolygon(val pg: Polygon) extends Iterable[Coordinate] {
    override def iterator: Iterator[Coordinate] = pg.getExteriorRing.getCoordinates.toIterator

    def getHoles: List[Polygon] = {
      val holes: IndexedSeq[LineString] = for {i <- 0 until pg.getNumInteriorRing }
        yield pg.getInteriorRingN(i)

      holes.map(ls => createPolygon(ls.getCoordinates)).toList
    }

    def getHolesCoordinates: List[List[Coordinate]] = {
      val holes: IndexedSeq[LineString] = for {i <- 0 until pg.getNumInteriorRing }
        yield pg.getInteriorRingN(i)

      holes.map(_.getCoordinates.toList).toList
    }
  }

  def normalizePolygon(pg: Polygon): Polygon = pg.norm.asInstanceOf[Polygon]
  def createPolygon(coordinates: Array[Coordinate]): Polygon = {
    val polygon = geometryFactory.createPolygon(coordinates)
    polygon.normalize()
    polygon
  }
}
