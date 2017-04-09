package org.jeom

import scala.collection.JavaConverters._

import com.vividsolutions.jts.algorithm.Angle
import com.vividsolutions.jts.densify.Densifier
import com.vividsolutions.jts.simplify.DouglasPeuckerSimplifier
import com.vividsolutions.jts.triangulate.DelaunayTriangulationBuilder
import com.vividsolutions.jts.geom.{GeometryFactory, Geometry, Polygon, Coordinate, LineString}
import com.vividsolutions.jts.operation.union.CascadedPolygonUnion


class OrthogonalPolygon(polygon: Polygon) {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")
  }
}


case class Vec(coordinate: Coordinate, angle: Double)


object Vec {
  var angles: Set[Double] = Set(0.0, 90.0, 180.0, -90.0, 270.0)

  def apply(a: List[Coordinate]) = 
    new Vec(a(1), Angle.toDegrees(Angle.angle(a.head, a.tail.head)))

  def vecBasisFolder(a: List[Vec], b: Vec): List[Vec] =
    if (a.head.angle == b.angle && angles.contains(b.angle)) b :: a.tail else b :: a

  def vecColinearFolder(a: List[Vec], b: Vec): List[Vec] =
    if (a.head.angle == b.angle) b :: a.tail else b :: a
}


object PolygonSimplifier {
  import GeometryUtils.IterablePolygon
  val geometryFactory = new GeometryFactory()

  private def polygon2vecs(pg: Polygon): List[Vec] = {
    pg.toList
      .sliding(2, 1)
      .map(Vec.apply)
      .toList
  }

  private def filterVecs(folder: (List[Vec], Vec) => List[Vec])(vecs: List[Vec])
    : List[Vec] = {

    val reduced: List[Vec] = vecs
      .tail
      .foldLeft(List(vecs.head))(folder)

    reduced :+ reduced.head
  }

  private def vecs2polygon(vecs: List[Vec]): Polygon = {
    geometryFactory
      .createPolygon(vecs.map(_.coordinate).toArray)
      .norm
      .asInstanceOf[Polygon]
  }

  def removeColinearity: Polygon => Polygon = {
    polygon2vecs _ andThen 
    filterVecs(Vec.vecColinearFolder) _ andThen
    vecs2polygon _
  }

  def removeBasisColinearity: Polygon => Polygon = {
    polygon2vecs _ andThen 
    filterVecs(Vec.vecBasisFolder) _ andThen 
    vecs2polygon _
  }
}


object OrthogonalPolygonBuilder {
  import GeometryUtils.IterablePolygon

  val tol: Double = scala.math.pow(2, -12)
  val geometryFactory = new GeometryFactory()

  def coverCoordinates(points: Iterable[Coordinate]): Geometry = {
    geometryFactory
      .createLineString(points.toArray)
      .getEnvelope
  }

  def build(
      polygon: Polygon,
      tolerance: Double = tol,
      size: Int = 3,
      step: Int = 1,
      densify: Boolean = false): Polygon = {

    val poly: Polygon = densify match {
      case false => DouglasPeuckerSimplifier
        .simplify(polygon, tolerance.max(0))
        .asInstanceOf[Polygon]

      case true => Densifier
        .densify(polygon, tolerance.max(0))
        .asInstanceOf[Polygon]
    }

    val simpler: Polygon = PolygonSimplifier.removeBasisColinearity(poly)

    val length: Int = size.max(3)
    val window: Int = step.min(length - 2).max(1)

    val coveringRectangles: List[Geometry] = simpler
      .sliding(length, window)
      .map(coverCoordinates)
      .toList

    val newBoundary: LineString = CascadedPolygonUnion
      .union(coveringRectangles.asJavaCollection)
      .asInstanceOf[Polygon]
      .getExteriorRing

    PolygonSimplifier removeColinearity
      geometryFactory.createPolygon(newBoundary.getCoordinates)
  }
}
