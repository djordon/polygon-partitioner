package org.partitioner.orthogonal

import com.vividsolutions.jts.geom.Polygon
import org.partitioner._


private[partitioner] object partitionBasic extends Function1[Polygon, List[Rectangle]] {

  def literal: Polygon => List[Rectangle] = (extractCorners _)
    .andThen(makeRectangleCorners _)
    .andThen(extractRectangles _)

  def apply(polygon: Polygon): List[Rectangle] = literal(polygon)
}


/**
 * Partitions an orthogonal polygon into a list of non-overlapping
 * rectangles.
 *
 * @param polygon the orthogonal polygon to partition
 * @return Returns a list of non-overlapping rectangles
 */
object partition extends Function1[Polygon, List[Rectangle]] {

  def apply(polygon: Polygon): List[Rectangle] =
    partitionBasic { removeAxisAlignedCollinearity(polygon) }
}
