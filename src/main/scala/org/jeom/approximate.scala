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
  val angles: Set[Double] = Set(0.0, 90.0, 180.0, -90.0, 270.0)
  val epsilon: Double = 1.1102230246251568E-11

  def apply(a: List[Coordinate]) = 
    new Vec(a(1), Angle.toDegrees(Angle.angle(a.head, a.tail.head)))

  def vecBasisFolder(a: List[Vec], b: Vec): List[Vec] = {
    if ((a.head.angle - b.angle).abs < epsilon && angles.contains(b.angle)) 
      b :: a.tail
    else
      b :: a
  }

  def vecColinearFolder(a: List[Vec], b: Vec): List[Vec] =
    if ((a.head.angle - b.angle).abs < epsilon) b :: a.tail else b :: a
}


object PolygonApproximator {
  import GeometryUtils.IterablePolygon
  val geometryFactory = new GeometryFactory()

  def polygon2vecs(pg: Polygon): List[Vec] = {
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

  def removeAxisAlignedColinearity: Polygon => Polygon = {
    polygon2vecs _ andThen 
    filterVecs(Vec.vecBasisFolder) _ andThen 
    vecs2polygon _
  }

  def simplify(polygon: Polygon, tolerance: Double): Polygon = DouglasPeuckerSimplifier
    .simplify(polygon, tolerance)
    .norm()
    .asInstanceOf[Polygon]

  def densify(polygon: Polygon, tolerance: Double): Polygon = Densifier
    .densify(polygon, tolerance)
    .norm()
    .asInstanceOf[Polygon]
}


object OrthogonalPolygonBuilder {
  import GeometryUtils.IterablePolygon
  import PolygonApproximator.{densify, removeColinearity, simplify}

  val tol: Double = scala.math.pow(2, -12)
  val geometryFactory = new GeometryFactory()

  def coverCoordinates(points: Iterable[Coordinate]): Geometry = {
    geometryFactory
      .createLineString(points.toArray)
      .getEnvelope
  }

  def cover(polygon: Polygon, size: Int = 3, step: Int = 1): Polygon = {
    val simpler: Polygon = PolygonApproximator.removeAxisAlignedColinearity(polygon)
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

    removeColinearity(geometryFactory.createPolygon(newBoundary.getCoordinates))
  }

  def approximate(
      polygon: Polygon,
      simplifyTolerance: Double = tol,
      densifyTolerance: Double = 10.0,
      size: Int = 3,
      step: Int = 1): Polygon = {

    val method: Polygon => Polygon = Function.chain(Seq(
      simplify(_: Polygon, simplifyTolerance),
      densify(_: Polygon, densifyTolerance),
      cover(_: Polygon, size, step)
    ))

    method(polygon)
  }
}
