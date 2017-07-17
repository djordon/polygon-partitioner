package org.partitioner.orthogonal

import com.vividsolutions.jts.geom.Polygon
import org.partitioner.{Rectangle, removeAxisAlignedCollinearity}


/**
 * Partitions an orthogonal polygon into a list of non-overlapping
 * rectangles.
 *
 * @param polygon the orthogonal polygon to partition
 * @return Returns a list of non-overlapping rectangles
 */
object partition extends Function1[Polygon, List[Rectangle]] {

  def apply(polygon: Polygon): List[Rectangle] = removeAxisAlignedCollinearity
    .andThen(extractCorners)
    .andThen(makeRectangleCorners)
    .andThen(extractRectangles)(polygon)
}
