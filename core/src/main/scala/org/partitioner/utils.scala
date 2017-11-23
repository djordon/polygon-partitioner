package org.partitioner

import org.locationtech.jts.geom._
import org.locationtech.jts.io.WKTReader

import scala.io.Source


object GeometryUtils {
  lazy val geometryFactory = new GeometryFactory()
  lazy val wktReader = new WKTReader()

  implicit class IterablePolygon(val pg: Polygon) extends Iterable[Coordinate] {
    override def iterator: Iterator[Coordinate] = pg.getExteriorRing.getCoordinates.toIterator

    def getHoles: List[Polygon] = {
      val holes: IndexedSeq[LineString] = for {i <- 0 until pg.getNumInteriorRing }
        yield pg.getInteriorRingN(i)

      holes.map(ls => geometryFactory.createPolygon(ls.getCoordinates)).toList
    }

    def getHolesCoordinates: List[List[Coordinate]] = {
      val holes: IndexedSeq[LineString] = for {i <- 0 until pg.getNumInteriorRing }
        yield pg.getInteriorRingN(i)

      holes.map(_.getCoordinates.toList).toList
    }
  }

  def normalizePolygon(pg: Polygon): Polygon = pg.norm.asInstanceOf[Polygon]

  def loadResources(dir: String) : Map[String, Polygon] = Source
    .fromResource(dir)
    .getLines
    .map(f => (f, Source.fromResource(s"$dir/$f").mkString))
    .toMap
    .mapValues(wktReader.read(_).asInstanceOf[Polygon])
}


private[partitioner] object coverCoordinates
  extends Function1[Iterable[Coordinate], Geometry] {

  def apply(points: Iterable[Coordinate]): Geometry = {
    GeometryUtils.geometryFactory.createLineString(points.toArray).getEnvelope
  }
}
