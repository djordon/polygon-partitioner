package org.partitioner

import com.vividsolutions.jts.geom.{LinearRing, Polygon}
import org.partitioner.GeometryUtils.{IterablePolygon, geometryFactory}


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
object cover extends Function3[Polygon, Int, Int, Polygon] {

  def apply(polygon: Polygon, size: Int = 3, step: Int = 1): Polygon = {
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
