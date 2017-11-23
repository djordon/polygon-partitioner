package org.partitioner

import org.locationtech.jts.densify.Densifier
import org.locationtech.jts.geom.{Geometry, Polygon}


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
object densify extends Function2[Polygon, Double, Polygon] {

  def apply(polygon: Polygon, tolerance: Double): Polygon = {
    if (0 < tolerance && tolerance < Double.PositiveInfinity) {
      val newPolygon: Geometry = Densifier.densify(polygon.norm, tolerance)
      newPolygon.normalize()
      newPolygon.asInstanceOf[Polygon]
    } else
      polygon.norm.asInstanceOf[Polygon]
  }
}
