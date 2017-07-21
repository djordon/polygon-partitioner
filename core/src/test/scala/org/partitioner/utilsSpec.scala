package org.partitioner

import org.scalatest.{Matchers, WordSpec}
import com.vividsolutions.jts.geom.Polygon



class GeometryUtilsSpec extends WordSpec with Matchers with PolygonFixtures {
  import orthogonal.isOrthogonalPolygon

  "loadResources" can {
    "load polygons from the resources folder" in {

      val polygons1: Map[String, Polygon] = GeometryUtils.loadResources("orthogonal-polygons")
      polygons1.toList.length should be > 0
      polygons1.values.map(isOrthogonalPolygon).reduce(_ && _) should be (true)

      val polygons2: Map[String, Polygon] = GeometryUtils.loadResources("non-orthogonal-polygon")
      polygons2.toList.length should be > 0
      polygons2.values.map(isOrthogonalPolygon).reduce(_ && _) should be (false)

    }
  }

  "isOrthogonalPolygon" can {
    "tell if the input polygon is orthogonal or not" in {
      for (pg <- nonOrthogonalPolygonFixtures.values)
        isOrthogonalPolygon(pg) should be (false)

      for (pg <- orthogonalPolygonFixtures.values)
        isOrthogonalPolygon(pg) should be (true)
    }
  }
}