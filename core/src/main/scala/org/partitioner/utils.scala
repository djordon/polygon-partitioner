package org.partitioner

import com.vividsolutions.jts.geom._
import com.vividsolutions.jts.io.WKTReader

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

  /**
   * Returns a boolean the input polygon is an orthogonal polygon
   *
   * @param polygon the input polygon
   *
   * @return A boolean indicating whether the input is an orthogonal polygon
   */
  def isOrthogonalPolygon(polygon: Polygon): Boolean = {
    polygon
      .toList
      .sliding(2, 1)
      .collect { case a :: b :: Nil => a.x == b.x || a.y == b.y }
      .reduce(_ && _)
  }

  def loadResources(dir: String) : Map[String, Polygon] = Source
    .fromResource(dir)
    .getLines
    .map(f => (f, Source.fromResource(s"$dir/$f").mkString))
    .toMap
    .mapValues(wktReader.read(_).asInstanceOf[Polygon])
}
