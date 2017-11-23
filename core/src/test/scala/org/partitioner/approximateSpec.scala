package org.partitioner

import org.scalatest.WordSpec
import org.scalactic.TolerantNumerics
import org.locationtech.jts.algorithm.Angle
import org.locationtech.jts.geom.{Coordinate, MultiPoint, Polygon}


case class Vertex(coord: Coordinate, angle: Double)


object Vertex {
  def apply(coordinates: Iterable[Coordinate]) = coordinates match {
    case b :: c :: Nil => new Vertex(c, Angle.toDegrees(Angle.angle(b, c)))
  }
}


class PolygonApproximationSpec extends WordSpec with PolygonFixtures {
  val epsilon: Double = 1.1102230246251568E-16
  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(epsilon)

  "PolygonApproximator" can {
    import GeometryUtils.normalizePolygon

    "removeAxisAlignedCollinearity" should {
      "remove redundant points that lie on axis aligned lines" in {
        val simplified: Polygon = removeAxisAlignedCollinearity(fixtures("redundantBasisPointPolygon"))

        assert { simplified == fixtures("simplePolygon") }
      }

      "keep redundant points that lie on non-axis aligned lines" in {
        val simplified: Polygon = removeAxisAlignedCollinearity(fixtures("redundantPointPolygon"))

        val nothingHappened: Polygon = removeAxisAlignedCollinearity(fixtures("preDensifiedPolygon"))

        assert { simplified == fixtures("lessRedundantPointPolygon") }
        assert { nothingHappened == fixtures("preDensifiedPolygon") }
      }
    }

    "densify" should {
      "Do nothing when the tolerance is positive infinity or zero" in {
        for (pg <- fixtures.values) {
          val newPolygon1 = densify(pg, Double.PositiveInfinity)
          assert { newPolygon1 == normalizePolygon(pg) }

          val newPolygon2 = densify(pg, 0.0)
          assert { newPolygon2 == normalizePolygon(pg) }
        }
      }

      "Increase the number of points along the boundary" in {
        for (pg <- fixtures.values) {
          val newPolygon = densify(pg, 0.1)
          assert(newPolygon.getNumPoints >= pg.getNumPoints)
        }
      }
    }

    "simplify" should {
      "Do nothing when the tolerance is positive infinity or less than zero" in {
        for (pg <- fixtures.values) {
          val newPolygon1 = simplify(pg, Double.PositiveInfinity)
          assert { newPolygon1 == normalizePolygon(pg) }

          val newPolygon2 = simplify(pg, -1)
          assert { newPolygon2 == normalizePolygon(pg) }
        }
      }

      "reduce the number of points along the boundary and preserve holes when preserve is true" in {
        for (pg <- fixtures.values) {
          val newPolygon1 = simplify(pg, 0.01, true)
          assert(newPolygon1.getNumPoints <= pg.getNumPoints)
          assert(newPolygon1.getNumInteriorRing == pg.getNumInteriorRing)

          val newPolygon2 = simplify(pg, 0.01, false)
          assert(newPolygon2.getNumPoints <= pg.getNumPoints)
        }
      }
    }
  }
}

class OrthogonalPolygonBuilderSpec extends WordSpec with PolygonFixtures {

  "OrthogonalPolygonBuilder" can {
     import GeometryUtils.IterablePolygon

    "coverCoordinates" should {
      "create a geometry that covers the input coordinates" in {
        for (i <- 0 until 10) {
          randomGeometryFactory.setNumPoints(20)

          val points = randomGeometryFactory
            .getGeometry
            .asInstanceOf[MultiPoint]
            .getCoordinates
            .filter(p => Math.random() > 0.5)

          val cover = coverCoordinates(points)

          assert { cover.contains(geometryFactory.createLineString(points)) }
        }
      }
    }

    "createExteriorCover" should {
      "create the expected polygon" in {
        val covered: Polygon = createExteriorCover(fixtures("preDensifiedPolygon"))

        assert { covered == fixtures("approximatedPolygon") }
      }

      "create an orthogonal polygon" in {
        for (i <- 0 until 10) {
          val randomPolygon: Polygon = generatePolygon()
          val randomCover: Polygon = createExteriorCover(randomPolygon)
          val axisAlignedAngles: Set[Double] = Set(0.0, 90.0, 180.0, -90.0, 270.0)

          val vecs: List[Vertex] = randomCover
            .toList
            .sliding(2, 1)
            .map(Vertex.apply)
            .toList

          assert { vecs.forall(v => axisAlignedAngles.contains(v.angle)) }
        }
      }

      "cover the polygon" in {
        val covered: Polygon = createExteriorCover(fixtures("preDensifiedPolygon"))

        val randomPolygon: Polygon = generatePolygon()
        val randomCover: Polygon = createExteriorCover(randomPolygon)

        assert { covered covers fixtures("preDensifiedPolygon") }
        assert { randomCover covers randomPolygon }
      }
    }

    "cover" should {
      "create the expected polygon" in {
        val covered: Polygon = cover(fixtures("preDensifiedPolygon"))

        assert { covered == fixtures("approximatedPolygon") }
      }

      "create an orthogonal polygon" in {
        for (i <- 0 until 10) {
          val randomPolygon: Polygon = generatePolygon()
          val randomCover: Polygon = cover(randomPolygon)
          val axisAlignedAngles: Set[Double] = Set(0.0, 90.0, 180.0, -90.0, 270.0)

          val vecs: List[Vertex] = randomCover
            .toList
            .sliding(2, 1)
            .map(Vertex.apply)
            .toList

          assert { vecs.forall(v => axisAlignedAngles.contains(v.angle)) }
        }
      }

      "cover the polygon" in {
        for (pg <- fixtures.values) {
          val covered: Polygon = cover(pg)

          val randomPolygon: Polygon = generatePolygon()
          val randomCover: Polygon = cover(randomPolygon)

          assert { covered covers pg }
          assert { randomCover covers randomPolygon }
        }
      }
    }

    "createInteriorCover" should {
      "return polygons that lay within the original polygon" in {
        for ((name, polygon) <- fixtures) {
          val interior: List[Polygon] = createInteriorCover(polygon)

          val exterior: Polygon = geometryFactory.createPolygon(polygon.getExteriorRing.getCoordinates)

          assert { interior.forall(pg => exterior contains pg) }
        }
      }
    }
  }
}

