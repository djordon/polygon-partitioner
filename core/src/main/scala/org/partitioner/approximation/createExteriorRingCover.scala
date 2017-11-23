package org.partitioner

import GeometryUtils.IterablePolygon
import scala.collection.JavaConverters._
import org.locationtech.jts.geom.{Geometry, Polygon}
import org.locationtech.jts.operation.union.CascadedPolygonUnion


object createExteriorRingCover extends Function3[Polygon, Int, Int, Polygon] {

  def apply(polygon: Polygon, size: Int = 3, step: Int = 1): Polygon = {
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
}
