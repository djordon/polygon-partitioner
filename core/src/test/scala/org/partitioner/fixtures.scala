package org.partitioner

import com.vividsolutions.jts.geom.{Polygon, GeometryFactory}
import com.vividsolutions.jts.geom.util.AffineTransformation
import com.vividsolutions.jts.io.WKTReader
import com.vividsolutions.jts.shape.random.RandomPointsBuilder

import scala.io.Source


trait PolygonFixtures {
  import GeometryUtils.normalizePolygon

  val wktReader = new WKTReader()

  val geometryFactory = new GeometryFactory()
  val randomGeometryFactory = new RandomPointsBuilder()

  def generatePolygon(numPoints: Int = 50): Polygon = {
    randomGeometryFactory.setNumPoints(numPoints)

    randomGeometryFactory
      .getGeometry
      .convexHull
      .norm
      .asInstanceOf[Polygon]
  }

  def loadDirectory(dir: String) : Map[String, Polygon] = Source
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
    rotatePolygons(loadDirectory("rectilinear"))

  lazy val nonOrthogonalPolygonFixtures: Map[String, Polygon] = loadDirectory("non-rectilinear")
  lazy val fixtures = orthogonalPolygonFixtures ++ nonOrthogonalPolygonFixtures
}
