package org.partitioner

import org.locationtech.jts.geom.{GeometryFactory, Polygon}
import org.locationtech.jts.geom.util.AffineTransformation
import org.locationtech.jts.io.WKTReader
import org.locationtech.jts.shape.random.RandomPointsBuilder

import scala.io.Source

trait PolygonFixtures {
  import GeometryUtils.normalizePolygon

  val geometryFactory = new GeometryFactory()
  val randomGeometryFactory = new RandomPointsBuilder()
  val wktReader = new WKTReader()

  def generatePolygon(numPoints: Int = 50): Polygon = {
    randomGeometryFactory.setNumPoints(numPoints)

    randomGeometryFactory
      .getGeometry
      .convexHull
      .norm
      .asInstanceOf[Polygon]
  }

  def loadResources(dir: String) : Map[String, Polygon] = Source
    .fromResource(dir)
    .getLines
    .map(f => (f, Source.fromResource(s"$dir/$f").mkString))
    .toMap
    .mapValues(wktReader.read(_).asInstanceOf[Polygon])

  def rotatePolygons(polygonMap: Map[String, Polygon]): Map[String, Polygon] = {
    val matrices = Map(
      "" -> Array(1.0, 0.0, 0.0, 0.0, 1.0, 0.0),
      "-rotated-90" -> Array(0, -1.0, 0.0, 1.0, 0, 0.0),
      "-rotated-180" -> Array(-1.0, 0, 0.0, 0, -1.0, 0.0),
      "-rotated-270" -> Array(0, 1.0, 0.0, -1.0, 0, 0.0)
    )
    for {
      (filename, polygon) <- polygonMap
      (angle, matrix)  <- matrices
      af = new AffineTransformation(matrix)
    } yield (s"$filename$angle", normalizePolygon(af.transform(polygon).asInstanceOf[Polygon]))
  }

  lazy val orthogonalPolygonFixtures: Map[String, Polygon] =
    rotatePolygons(loadResources("orthogonal-polygons"))

  lazy val nonOrthogonalPolygonFixtures: Map[String, Polygon] =
    loadResources("non-orthogonal-polygon")

  lazy val fixtures = orthogonalPolygonFixtures ++ nonOrthogonalPolygonFixtures
}
