package org.partitioner

import com.vividsolutions.jts.geom.Polygon
import org.partitioner.GeometryUtils.geometryFactory


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
object createExteriorCover extends Function3[Polygon, Int, Int, Polygon] {

  def apply(polygon: Polygon, size: Int = 3, step: Int = 1): Polygon = {

    val boundaryCover: Polygon = createExteriorRingCover(polygon, size, step)
    geometryFactory.createPolygon(boundaryCover.getExteriorRing.getCoordinates)
  }
}
