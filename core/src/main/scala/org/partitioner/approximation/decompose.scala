package org.partitioner

import com.vividsolutions.jts.geom.Polygon
import org.partitioner.orthogonal.partitionBasic


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
object decompose extends Function3[Polygon, Int, Int, List[Rectangle]] {

  def apply(polygon: Polygon, size: Int = 3, step: Int = 1): List[Rectangle] = {
    partitionBasic { cover(polygon, size, step) }
  }
}