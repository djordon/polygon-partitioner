package org.partitioner

import scala.annotation.switch
import scala.collection.JavaConverters._
import com.vividsolutions.jts.densify.Densifier
import com.vividsolutions.jts.simplify.{DouglasPeuckerSimplifier, TopologyPreservingSimplifier}
import com.vividsolutions.jts.geom.{Coordinate, Geometry, LinearRing, Polygon}
import com.vividsolutions.jts.operation.union.CascadedPolygonUnion
import GeometryUtils.{IterablePolygon, geometryFactory}


object PolygonAugmenter {
  def polygon2Vertices(pg: Polygon): List[Coordinate] = {
    val boundary: List[Coordinate] = pg.toList
    Stream
      .continually(boundary)
      .flatten
      .take(boundary.length * 2)
      .toList
  }

  private[this] def filterVertices(vertices: List[Coordinate]): List[Coordinate] = {
    val reduced: List[Coordinate] = vertices
      .drop(2)
      .foldLeft(vertices.take(2))(rectilinearFolder)

    val boundary: List[Coordinate] = reduced.take(reduced.length / 2 + 1).tail
    val last: Coordinate = boundary.last

    if (last == boundary.head) boundary else last :: boundary
  }

  @inline
  private[this] def isAxisAligned(co: List[Coordinate]): Boolean = (co: @unchecked) match {
    case a :: b :: c :: Nil => (a.x == c.x && b.x == c.x) || (a.y == c.y && b.y == c.y)
  }

  private[this] def rectilinearFolder(a: List[Coordinate], b: Coordinate): List[Coordinate] = {
    if (isAxisAligned { b :: a.take(2) }) b :: a.tail else b :: a
  }

  private[this] def vertices2LinearRing(vertices: List[Coordinate]): LinearRing = {
    geometryFactory.createLinearRing(vertices.toArray)
  }

  private[this] def removeAxisAlignedCollinearitySimple: Polygon => LinearRing = {
    polygon2Vertices _ andThen filterVertices _ andThen vertices2LinearRing _
  }

  /**
   * Returns a topologically equivalent polygon where axis aligned line
   * segments are as long as possible.
   *
   * A polygon can contain superfluous points along the boundary, where
   * removing such points does not change the shape. This method removes
   * instances where these superfluous points occur on axis aligned line
   * segments.
   *
   * The input polygon is not modified.
   *
   * An example of this is where the input is a rectangle with 5 points
   * (where a point was added at the midpoint of the top edge). This
   * function would return the same rectangle, but with 4 points.
   *
   * @param polygon the input polygon
   * @return an modified version of the input polygon.
   */
  def removeAxisAlignedCollinearity(polygon: Polygon): Polygon = {
    val shell: LinearRing = removeAxisAlignedCollinearitySimple(polygon)
    val holes: List[LinearRing] = polygon
      .getHoles
      .map(removeAxisAlignedCollinearitySimple)

    val pg: Polygon = geometryFactory.createPolygon(shell, holes.toArray)
    pg.normalize()
    pg
  }

  /**
   * Returns a simplified version of the input polygon.
   *
   * Under the hood this calls the simplify method in
   * DouglasPeuckerSimplifier or TopologyPreservingSimplifier
   * from JTS. When preserve is false DouglasPeuckerSimplifier is used.
   * This means the output can be split, the output can collapse into
   * lines or disappear entirely, holes can be created or expected holes
   * can disappear, and lines can cross. On the plus side, it is much
   * faster than the TopologyPreservingSimplifier based approached
   * that corresponds to when preserve is true.
   *
   * @param polygon The input polygon.
   * @param tolerance The tolerance to use when simplifying the boundary.
   *                  Must be non negative. Greater values imply a coarser
   *                  (less accurate) output polygon.
   * @param preserve Specifies whether the simplifying algorithm should ensure
   *                 that the topology of the input polygon is preserved.
   *
   * @return Returns a simplified polygon that has been normalized.
   */
  def simplify(
      polygon: Polygon,
      tolerance: Double,
      preserve: Boolean = false): Polygon = {

    val simplifier = (preserve: @switch) match {
      case true => TopologyPreservingSimplifier.simplify _
      case false => DouglasPeuckerSimplifier.simplify _
    }

    if (0 <= tolerance && tolerance < Double.PositiveInfinity) {
      val newPolygon: Geometry = simplifier(polygon.norm, tolerance)
      newPolygon.normalize()
      newPolygon.asInstanceOf[Polygon]
    } else
      polygon.norm.asInstanceOf[Polygon]
  }

  /**
   * Returns a polygon that has points along the boundary added to it.
   *
   * The input polygon is not modified.
   *
   * @param polygon The input polygon.
   * @param tolerance Specifies the distance tolerance when adding points
   *                  to the boundary.
   *
   * @return Returns a list of non-overlapping rectangles.
   */
  def densify(polygon: Polygon, tolerance: Double): Polygon = {
    if (0 < tolerance && tolerance < Double.PositiveInfinity) {
      val newPolygon: Geometry = Densifier.densify(polygon.norm, tolerance)
      newPolygon.normalize()
      newPolygon.asInstanceOf[Polygon]
    } else
      polygon.norm.asInstanceOf[Polygon]
  }
}


object OrthogonalPolygonBuilder {
  import PolygonAugmenter.removeAxisAlignedCollinearity

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
   * @param polygon the input polygon.
   * @param size a value that sets how coarse the output should be.
   *             The larger the value, the more coarse the out put will be.
   *             Must be greater than 2.
   * @param step a value that helps set how coarse the output should be.
   *             The larger the value, the more coarse the out put will be.
   *             Must be greater than 0 and less size - 1.
   *
   * @return Returns an orthogonal polygon.
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

  /**
   * Creates an orthogonal polygon that covers the input polygon.
   *
   * This function attempts to find an orthogonal polygon that has as
   * many holes as the input polygon, but it does not always succeed in
   * doing so. Because this method attempts to accommodate holes while
   * covering the polygon, it can be quite computational intensive
   * compared to other methods in this package.
   *
   * @param polygon the input polygon.
   * @param size a value that sets how coarse the output should be.
   *             The larger the value, the more coarse the out put will be.
   *             Must be greater than 2.
   * @param step a value that helps set how coarse the output should be.
   *             The larger the value, the more coarse the out put will be.
   *             Must be greater than 0 and less size - 1.
   * @return returns an orthogonal polygon.
   */
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

    val orthogonalPolygon: Polygon = geometryFactory.createPolygon(exterior, holes)
    orthogonalPolygon.normalize()
    orthogonalPolygon
  }
}


object PolygonPartitioner {
  import orthogonal.OrthogonalPolygonPartitioner.partitionLiteral
  import OrthogonalPolygonBuilder.cover

  /**
    * Returns a list of non-overlapping rectangles that cover the input
    * polygon.
    *
    * Holes in the input polygon need not be in output polygon. This method
    * can be really slow when holes are present.
    *
    * @param polygon The input polygon
    * @param size a value that sets how coarse the output should be.
    *             The larger the value, the more coarse the out put will be.
    *             Must be greater than 2.
    * @param step a value that helps set how coarse the output should be.
    *             The larger the value, the more coarse the out put will be.
    *             Must be greater than 0 and less size - 1.
    * @return Returns a list of non-overlapping rectangles
    */
  def partition(polygon: Polygon, size: Int = 3, step: Int = 1): List[Rectangle] =
    partitionLiteral { cover(polygon, size, step) }
}