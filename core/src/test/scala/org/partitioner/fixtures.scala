package org.partitioner

import com.vividsolutions.jts.geom.{Polygon, GeometryFactory}
import com.vividsolutions.jts.geom.util.AffineTransformation
import com.vividsolutions.jts.shape.random.RandomPointsBuilder


trait PolygonFixtures {
  import GeometryUtils.{normalizePolygon, loadResource}

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
    rotatePolygons(loadResource("rectilinear"))

  lazy val nonOrthogonalPolygonFixtures: Map[String, Polygon] = loadResource("non-rectilinear")
  lazy val fixtures = orthogonalPolygonFixtures ++ nonOrthogonalPolygonFixtures
}
