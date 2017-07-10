package org.partitioner

import org.scalatest.{Matchers, WordSpec}
import com.vividsolutions.jts.geom.Polygon



class GeometryUtilsSpec extends WordSpec with Matchers {
  import OrthogonalPolygonBuilder.isOrthogonalPolygon

  "loadResources" can {
    "load polygons from the resources folder" in {

      val polygons1: Map[String, Polygon] = GeometryUtils.loadResources("rectilinear")
      polygons1.toList.length should be > 0
      polygons1.values.map(isOrthogonalPolygon).reduce(_ && _) should be (true)

      val polygons2: Map[String, Polygon] = GeometryUtils.loadResources("non-rectilinear")
      polygons2.toList.length should be > 0
      polygons2.values.map(isOrthogonalPolygon).reduce(_ && _) should be (false)

    }
  }
}