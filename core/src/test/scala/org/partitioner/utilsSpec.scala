package org.partitioner

import org.scalatest.WordSpec
import org.locationtech.jts.geom.Polygon



class GeometryUtilsSpec extends WordSpec with PolygonFixtures {
  import orthogonal.isOrthogonalPolygon

  "loadResources" can {
    "load polygons from the resources folder" in {

      val polygons1: Map[String, Polygon] = GeometryUtils.loadResources("orthogonal-polygons")
      assert { polygons1.toList.length > 0 }
      assert { polygons1.values.forall(isOrthogonalPolygon) }

      val polygons2: Map[String, Polygon] = GeometryUtils.loadResources("non-orthogonal-polygon")
      assert { polygons2.toList.length > 0 }
      assert { !polygons2.values.forall(isOrthogonalPolygon) }

    }
  }

  "isOrthogonalPolygon" can {
    "tell if the input polygon is orthogonal or not" in {
      for (pg <- nonOrthogonalPolygonFixtures.values)
        assert { !isOrthogonalPolygon(pg) }

      for (pg <- orthogonalPolygonFixtures.values)
        assert { isOrthogonalPolygon(pg) }
    }
  }
}