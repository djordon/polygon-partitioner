package org.partitioner.orthogonal

import org.partitioner.GeometryUtils.IterablePolygon
import com.vividsolutions.jts.geom.Polygon


/**
 * Returns a boolean the input polygon is an orthogonal polygon
 *
 * @param polygon the input polygon
 *
 * @return A boolean indicating whether the input is an orthogonal polygon
 */
object isOrthogonalPolygon extends Function1[Polygon, Boolean] {

  def apply(polygon: Polygon): Boolean = polygon
    .toList
    .sliding(2, 1)
    .collect { case a :: b :: Nil => a.x == b.x || a.y == b.y }
    .forall(_ == true)
}
