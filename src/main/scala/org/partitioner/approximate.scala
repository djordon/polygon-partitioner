package org.partitioner

import scala.collection.JavaConverters._

import com.vividsolutions.jts.densify.Densifier
import com.vividsolutions.jts.simplify.DouglasPeuckerSimplifier
import com.vividsolutions.jts.geom.{GeometryFactory, Geometry, Polygon, Coordinate, LineString, LinearRing}
import com.vividsolutions.jts.operation.union.CascadedPolygonUnion


object PolygonApproximator {
  import GeometryUtils.IterablePolygon
  val geometryFactory = new GeometryFactory()

  private def polygon2vecs(pg: Polygon): List[Vec] = {
    val boundary: List[Coordinate] = pg.toList
    Stream
      .continually(boundary)
      .flatten
      .take(boundary.length * 2)
      .toList
      .sliding(2, 1)
      .map(Vec.apply)
      .toList
  }

  private def filterVecs(vecs: List[Vec]): List[Vec] = {
    val reduced: List[Vec] = vecs
      .drop(2)
      .foldLeft(vecs.take(2))(rectilinearFolder)

    val boundary: List[Vec] = reduced.take(reduced.length / 2 + 1).tail
    val last: Vec = boundary.last

    if (last == boundary.head) boundary else last :: boundary
  }

  private def isAxisAligned(a: List[Vec]): Boolean = {
    (a(0).coord.x == a(2).coord.x && a(1).coord.x == a(2).coord.x) ||
    (a(0).coord.y == a(2).coord.y && a(1).coord.y == a(2).coord.y)
  }

  private def rectilinearFolder(a: List[Vec], b: Vec): List[Vec] = {
    if (isAxisAligned { b :: a.take(2) }) b :: a.tail else b :: a
  }

  private def vecs2polygon(vecs: List[Vec]): Polygon = {
    geometryFactory
      .createPolygon(vecs.map(_.coord).toArray)
      .norm
      .asInstanceOf[Polygon]
  }

  def removeAxisAlignedColinearity: Polygon => Polygon = {
    polygon2vecs _ andThen filterVecs _ andThen vecs2polygon _
  }

  def simplify(polygon: Polygon, tolerance: Double): Polygon = DouglasPeuckerSimplifier
    .simplify(polygon, tolerance)
    .norm
    .asInstanceOf[Polygon]

  def densify(polygon: Polygon, tolerance: Double): Polygon = Densifier
    .densify(polygon, tolerance)
    .norm
    .asInstanceOf[Polygon]
}


object OrthogonalPolygonBuilder {
  import GeometryUtils.IterablePolygon
  import PolygonApproximator.{densify, removeAxisAlignedColinearity, simplify}

  val tol: Double = scala.math.pow(2, -12)
  val geometryFactory = new GeometryFactory()

  def coverCoordinates(points: Iterable[Coordinate]): Geometry = {
    geometryFactory
      .createLineString(points.toArray)
      .getEnvelope
  }

  def cover(polygon: Polygon, size: Int = 3, step: Int = 1): Polygon = {
    val simpler: Polygon = removeAxisAlignedColinearity(polygon)
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
    
    removeAxisAlignedColinearity(geometryFactory.createPolygon(newBoundary.getCoordinates))
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

    val exterior: Polygon = geometryFactory.createPolygon(polygon.toArray)
    val approximated: List[LinearRing] = (exterior :: polygon.getHoles)
      .map { method(_).getExteriorRing.asInstanceOf[LinearRing] }

    geometryFactory.createPolygon(approximated.head, approximated.tail.toArray)
  }
}
