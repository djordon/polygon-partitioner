package org.partitioner

import scala.collection.JavaConverters._
import com.vividsolutions.jts.densify.Densifier
import com.vividsolutions.jts.simplify.DouglasPeuckerSimplifier
import com.vividsolutions.jts.geom.{Polygon, Coordinate, LinearRing, Geometry}
import com.vividsolutions.jts.operation.union.CascadedPolygonUnion


object PolygonApproximator {
  import GeometryUtils.{IterablePolygon, geometryFactory}

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
    geometryFactory.createPolygon(vecs.map(_.coord).toArray)
  }

  def removeAxisAlignedColinearity(pg: Polygon): Polygon = {
    val shell: Polygon = removeAxisAlignedColinearitySimple(pg)
    val holes: List[Polygon] = pg
      .getHoles
      .map(removeAxisAlignedColinearitySimple)

    geometryFactory.createPolygon(
      shell.getExteriorRing.asInstanceOf[LinearRing],
      holes.map(_.getExteriorRing.asInstanceOf[LinearRing]).toArray
    ).norm.asInstanceOf[Polygon]
  }

  private def removeAxisAlignedColinearitySimple: Polygon => Polygon = {
    polygon2vecs _ andThen filterVecs _ andThen vecs2polygon _
  }

  def simplify(polygon: Polygon, tolerance: Double): Polygon = {
    if (0 <= tolerance && tolerance < Double.PositiveInfinity)
      DouglasPeuckerSimplifier
        .simplify(polygon, tolerance)
        .norm
        .asInstanceOf[Polygon]
    else
      polygon.norm.asInstanceOf[Polygon]
  }

  def densify(polygon: Polygon, tolerance: Double): Polygon = {
    if (0 < tolerance && tolerance < Double.PositiveInfinity)
      Densifier.densify(polygon, tolerance).norm.asInstanceOf[Polygon]
    else
      polygon.norm.asInstanceOf[Polygon]
  }
}


object OrthogonalPolygonBuilder {
  import GeometryUtils.{IterablePolygon, geometryFactory}
  import PolygonApproximator.{densify, removeAxisAlignedColinearity, simplify}

  val tol: Double = scala.math.pow(2, -12)

  def coverCoordinates(points: Iterable[Coordinate]): Geometry = {
    geometryFactory
      .createLineString(points.toArray)
      .getEnvelope
  }

  def createExteriorRingCover(
      polygon: Polygon,
      size: Int = 3,
      step: Int = 1): Polygon = {

    val simpler: Polygon = removeAxisAlignedColinearity(polygon)
    val length: Int = size.max(3)
    val window: Int = step.min(length - 2).max(1)

    val coveringRectangles: List[Geometry] = simpler
      .sliding(length, window)
      .map(coverCoordinates)
      .toList

    removeAxisAlignedColinearity {
      CascadedPolygonUnion
        .union(coveringRectangles.asJavaCollection)
        .asInstanceOf[Polygon]
    }
  }

  def createExteriorCover(
      polygon: Polygon,
      size: Int = 3,
      step: Int = 1): Polygon = {

    val boundaryCover: Polygon = createExteriorRingCover(polygon, size, step)
    geometryFactory.createPolygon(boundaryCover.getExteriorRing.getCoordinates)
  }

  def createInteriorCover(
      polygon: Polygon,
      size: Int = 3,
      step: Int = 1): List[Polygon] = {

    val ext = geometryFactory.createPolygon(polygon.getExteriorRing.getCoordinates)
    createExteriorRingCover(polygon, size, step).getHoles.filter(ext.covers)
  }

  def cover(polygon: Polygon, size: Int = 3, step: Int = 1): Polygon = {
    val boundaryCover: Polygon = createExteriorRingCover(polygon, size, step)

    val exterior: LinearRing = boundaryCover
      .getExteriorRing
      .asInstanceOf[LinearRing]

    val holes: Array[LinearRing] = polygon
      .getHoles
      .map(createExteriorRingCover(_: Polygon, size, step))
      .flatMap(_.getHoles.filter(polygon.covers))
      .map(_.getExteriorRing.asInstanceOf[LinearRing])
      .toArray

    geometryFactory
      .createPolygon(exterior, holes)
      .norm
      .asInstanceOf[Polygon]
  }

  def approximate(
      polygon: Polygon,
      simplifyTolerance: Double = tol,
      densifyTolerance: Double = Double.PositiveInfinity,
      size: Int = 3,
      step: Int = 1): Polygon = {

    val transformer: Polygon => Polygon = Function.chain(Seq(
      simplify(_: Polygon, simplifyTolerance),
      densify(_: Polygon, densifyTolerance),
      createExteriorCover(_: Polygon, size, step)
    ))

    val exterior: Polygon = geometryFactory.createPolygon(polygon.toArray)
    val approximated: List[LinearRing] = (exterior :: polygon.getHoles)
      .map { transformer(_).getExteriorRing.asInstanceOf[LinearRing] }

    geometryFactory
      .createPolygon(approximated.head, approximated.tail.toArray)
      .norm
      .asInstanceOf[Polygon]
  }
}
