package org.partitioner

import scala.annotation.switch
import scala.collection.JavaConverters._

import com.vividsolutions.jts.densify.Densifier
import com.vividsolutions.jts.simplify.{DouglasPeuckerSimplifier, TopologyPreservingSimplifier}
import com.vividsolutions.jts.geom.{Coordinate, Geometry, LinearRing, Polygon}
import com.vividsolutions.jts.operation.union.CascadedPolygonUnion

import GeometryUtils.{IterablePolygon, geometryFactory}


object PolygonApproximator {
  def polygon2Vertices(pg: Polygon): List[Vertex] = {
    val boundary: List[Coordinate] = pg.toList
    Stream
      .continually(boundary)
      .flatten
      .take(boundary.length * 2)
      .sliding(2, 1)
      .map(Vertex.apply)
      .toList
  }

  private[this] def filterVertices(vertices: List[Vertex]): List[Vertex] = {
    val reduced: List[Vertex] = vertices
      .drop(2)
      .foldLeft(vertices.take(2))(rectilinearFolder)

    val boundary: List[Vertex] = reduced.take(reduced.length / 2 + 1).tail
    val last: Vertex = boundary.last

    if (last == boundary.head) boundary else last :: boundary
  }

  private[this] def isAxisAligned(a: List[Vertex]): Boolean = {
    (a(0).coord.x == a(2).coord.x && a(1).coord.x == a(2).coord.x) ||
    (a(0).coord.y == a(2).coord.y && a(1).coord.y == a(2).coord.y)
  }

  private[this] def rectilinearFolder(a: List[Vertex], b: Vertex): List[Vertex] = {
    if (isAxisAligned { b :: a.take(2) }) b :: a.tail else b :: a
  }

  private[this] def vertices2polygon(vertices: List[Vertex]): Polygon = {
    geometryFactory.createPolygon(vertices.map(_.coord).toArray)
  }

  private[this] def removeAxisAlignedCollinearitySimple: Polygon => Polygon = {
    polygon2Vertices _ andThen filterVertices _ andThen vertices2polygon _
  }

  def removeAxisAlignedCollinearity(pg: Polygon): Polygon = {
    val shell: Polygon = removeAxisAlignedCollinearitySimple(pg)
    val holes: List[Polygon] = pg
      .getHoles
      .map(removeAxisAlignedCollinearitySimple)

    geometryFactory.createPolygon(
      shell.getExteriorRing.asInstanceOf[LinearRing],
      holes.map(_.getExteriorRing.asInstanceOf[LinearRing]).toArray
    ).norm.asInstanceOf[Polygon]
  }

  /**
   * Returns a simplified version of the input polygon.
   *
   * Under the hood this calls the simplify method in either
   * DouglasPeuckerSimplifier or TopologyPreservingSimplifier
   * from JTS. In general, the simplifier in DouglasPeuckerSimplifier
   * does not preserve topology. This means polygons can be split,
   * collapse to lines or disappear holes can be created or disappear,
   * and lines can cross. On the plus side, it is much faster than
   * the TopologyPreservingSimplifier.
   *
   * @param polygon The input polygon
   * @param tolerance The tolerance to use when simplifying the boundary.
   *                  Must be non negative. Greater values imply a coarser
   *                  (less accurate) output polygon.
   * @param preserve Specifies whether the simplifying algorithm should ensure
   *                 that the topology of the input polygon is preserved.
   *
   * @return Returns a simplified polygon that has been normalized
   */
  def simplify(
      polygon: Polygon,
      tolerance: Double,
      preserve: Boolean = false): Polygon = {

    val simplifier = (preserve: @switch) match {
      case true => TopologyPreservingSimplifier.simplify _
      case false => DouglasPeuckerSimplifier.simplify _
    }

    if (0 <= tolerance && tolerance < Double.PositiveInfinity)
      simplifier(polygon.norm, tolerance).norm.asInstanceOf[Polygon]
    else
      polygon.norm.asInstanceOf[Polygon]
  }

  /**
   * Returns a polygon that has points along the boundary added to it.
   *
   * @param polygon The input polygon
   * @param tolerance Specifies the distance tolerance when adding points
   *                  to the boundary.
   *
   * @return Returns a list of non-overlapping rectangles
   */
  def densify(polygon: Polygon, tolerance: Double): Polygon = {
    if (0 < tolerance && tolerance < Double.PositiveInfinity)
      Densifier.densify(polygon.norm, tolerance).norm.asInstanceOf[Polygon]
    else
      polygon.norm.asInstanceOf[Polygon]
  }
}


object OrthogonalPolygonBuilder {
  import PolygonApproximator.removeAxisAlignedCollinearity

  def coverCoordinates(points: Iterable[Coordinate]): Geometry = {
    geometryFactory.createLineString(points.toArray).getEnvelope
  }

  def createExteriorRingCover(
      polygon: Polygon,
      size: Int = 3,
      step: Int = 1): Polygon = {

    val simpler: Polygon = removeAxisAlignedCollinearity(polygon)
    val length: Int = size.max(3)
    val window: Int = step.min(length - 2).max(1)

    val coveringRectangles: List[Geometry] = simpler
      .sliding(length, window)
      .map(coverCoordinates)
      .toList

    removeAxisAlignedCollinearity {
      CascadedPolygonUnion
        .union(coveringRectangles.asJavaCollection)
        .asInstanceOf[Polygon]
    }
  }

  /**
   * Creates an orthogonal polygon that covers the input polygon
   *
   * @param polygon the input polygon
   * @param size a value that sets how coarse the output should be.
   *             The larger the value, the more coarse the out put will be.
   *             Must be greater than 2.
   * @param step a value that helps set how coarse the output should be.
   *             The larger the value, the more coarse the out put will be.
   *             Must be greater than 0 and less size - 1.
   *
   * @return Returns an orthogonal polygon
   */
  def createExteriorCover(
      polygon: Polygon,
      size: Int = 3,
      step: Int = 1): Polygon = {

    val boundaryCover: Polygon = createExteriorRingCover(polygon, size, step)
    geometryFactory.createPolygon(boundaryCover.getExteriorRing.getCoordinates)
  }

  private[partitioner] def createInteriorCover(
      polygon: Polygon,
      size: Int = 3,
      step: Int = 1): List[Polygon] = {

    val ext = geometryFactory.createPolygon(polygon.getExteriorRing.getCoordinates)
    createExteriorRingCover(polygon, size, step).getHoles.filter(ext.covers)
  }

  private[partitioner] def cover(polygon: Polygon, size: Int = 3, step: Int = 1): Polygon = {
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
}
