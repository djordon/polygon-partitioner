package org.partitioner

import org.locationtech.jts.geom.Polygon
import GeometryUtils.{geometryFactory, IterablePolygon}


private[partitioner] object createInteriorCover
  extends Function3[Polygon, Int, Int, List[Polygon]] {

  def apply(polygon: Polygon, size: Int = 3, step: Int = 1): List[Polygon] = {

    val ext = geometryFactory.createPolygon(polygon.getExteriorRing.getCoordinates)
    createExteriorRingCover(polygon, size, step).getHoles.filter(ext.covers)
  }
}
