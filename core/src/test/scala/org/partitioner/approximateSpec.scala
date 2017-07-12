package org.partitioner

import org.scalatest.{Matchers, WordSpec}
import org.scalactic.TolerantNumerics
import com.vividsolutions.jts.algorithm.Angle
import com.vividsolutions.jts.geom.{Coordinate, MultiPoint, Polygon}


case class Vertex(coord: Coordinate, angle: Double)


object Vertex {
  def apply(coordinates: Iterable[Coordinate]) = coordinates match {
    case b :: c :: Nil => new Vertex(c, Angle.toDegrees(Angle.angle(b, c)))
  }
}


class PolygonApproximationSpec extends WordSpec with Matchers with PolygonFixtures {
  val epsilon: Double = 1.1102230246251568E-16
  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(epsilon)

  "PolygonApproximator" can {
    import GeometryUtils.normalizePolygon

    "removeAxisAlignedCollinearity" should {
      "remove redundant points that lie on axis aligned lines" in {
        val simplified: Polygon = PolygonApproximator
          .removeAxisAlignedCollinearity(fixtures("redundantBasisPointPolygon"))

        simplified shouldEqual fixtures("simplePolygon")
      }

      "keep redundant points that lie on non-axis aligned lines" in {
        val simplified: Polygon = PolygonApproximator
          .removeAxisAlignedCollinearity(fixtures("redundantPointPolygon"))

        val nothingHappened: Polygon = PolygonApproximator
          .removeAxisAlignedCollinearity(fixtures("preDensifiedPolygon"))

        simplified shouldEqual fixtures("lessRedundantPointPolygon")
        nothingHappened shouldEqual fixtures("preDensifiedPolygon")
      }
    }

    "densify" should {
      "Do nothing when the tolerance is positive infinity or zero" in {
        for (pg <- fixtures.values) {
          val newPolygon1 = PolygonApproximator.densify(pg, Double.PositiveInfinity)
          newPolygon1 shouldEqual normalizePolygon(pg)

          val newPolygon2 = PolygonApproximator.densify(pg, 0.0)
          newPolygon2 shouldEqual normalizePolygon(pg)
        }
      }

      "Increase the number of points along the boundary" in {
        for (pg <- fixtures.values) {
          val newPolygon = PolygonApproximator.densify(pg, 0.1)
          newPolygon.getNumPoints should be >= pg.getNumPoints
        }
      }
    }

    "simplify" should {
      "Do nothing when the tolerance is positive infinity or less than zero" in {
        for (pg <- fixtures.values) {
          val newPolygon1 = PolygonApproximator.simplify(pg, Double.PositiveInfinity)
          newPolygon1 shouldEqual normalizePolygon(pg)

          val newPolygon2 = PolygonApproximator.simplify(pg, -1)
          newPolygon2 shouldEqual normalizePolygon(pg)
        }
      }

      "reduce the number of points along the boundary and preserve holes when preserve is true" in {
        for (pg <- fixtures.values) {
          val newPolygon1 = PolygonApproximator.simplify(pg, 0.01, true)
          newPolygon1.getNumPoints should be <= pg.getNumPoints
          newPolygon1.getNumInteriorRing shouldEqual (pg.getNumInteriorRing)

          val newPolygon2 = PolygonApproximator.simplify(pg, 0.01, false)
          newPolygon2.getNumPoints should be <= pg.getNumPoints
        }
      }
    }
  }
}

class OrthogonalPolygonBuilderSpec extends WordSpec with Matchers with PolygonFixtures {

  "OrthogonalPolygonBuilder" can {
     import GeometryUtils.{IterablePolygon, createPolygon}


    "isOrthogonalPolygon" can {
      "tell if the input polygon is orthogonal or not" in {
        for (pg <- nonOrthogonalPolygonFixtures.values)
          OrthogonalPolygonBuilder.isOrthogonalPolygon(pg) should be (false)

        for (pg <- orthogonalPolygonFixtures.values)
          OrthogonalPolygonBuilder.isOrthogonalPolygon(pg) should be (true)
      }
    }

    "coverCoordinates" should {
      "create a geometry that covers the input coordinates" in {
        for (i <- 0 until 10) {
          randomGeometryFactory.setNumPoints(20)

          val points = randomGeometryFactory
            .getGeometry
            .asInstanceOf[MultiPoint]
            .getCoordinates
            .filter(p => Math.random() > 0.5)

          val cover = OrthogonalPolygonBuilder.coverCoordinates(points)

          cover.contains(geometryFactory.createLineString(points)) should be(true)
        }
      }
    }

    "createExteriorCover" should {
      "create the expected polygon" in {
        val covered: Polygon = OrthogonalPolygonBuilder
          .createExteriorCover(fixtures("preDensifiedPolygon"))

        covered shouldEqual (fixtures("approximatedPolygon"))
      }

      "create an orthogonal polygon" in {
        for (i <- 0 until 10) {
          val randomPolygon: Polygon = generatePolygon()
          val randomCover: Polygon = OrthogonalPolygonBuilder.createExteriorCover(randomPolygon)
          val axisAlignedAngles: Set[Double] = Set(0.0, 90.0, 180.0, -90.0, 270.0)

          val vecs: List[Vertex] = randomCover
            .toList
            .sliding(2, 1)
            .map(Vertex.apply)
            .toList

          val isAxisAligned: List[Boolean] = vecs
            .map(v => axisAlignedAngles.contains(v.angle))

          isAxisAligned.reduce(_ && _) should be(true)
        }
      }

      "cover the polygon" in {
        val covered: Polygon = OrthogonalPolygonBuilder
          .createExteriorCover(fixtures("preDensifiedPolygon"))

        val randomPolygon: Polygon = generatePolygon()
        val randomCover: Polygon = OrthogonalPolygonBuilder.createExteriorCover(randomPolygon)

        covered covers fixtures("preDensifiedPolygon") should be (true)
        randomCover covers randomPolygon should be (true)
      }
    }

    "cover" should {
      "create the expected polygon" in {
        val covered: Polygon = OrthogonalPolygonBuilder
          .cover(fixtures("preDensifiedPolygon"))

        covered shouldEqual (fixtures("approximatedPolygon"))
      }

      "create an orthogonal polygon" in {
        for (i <- 0 until 10) {
          val randomPolygon: Polygon = generatePolygon()
          val randomCover: Polygon = OrthogonalPolygonBuilder.cover(randomPolygon)
          val axisAlignedAngles: Set[Double] = Set(0.0, 90.0, 180.0, -90.0, 270.0)

          val vecs: List[Vertex] = randomCover
            .toList
            .sliding(2, 1)
            .map(Vertex.apply)
            .toList

          val isAxisAligned: List[Boolean] = vecs
            .map(v => axisAlignedAngles.contains(v.angle))

          isAxisAligned.reduce(_ && _) should be(true)
        }
      }

      "cover the polygon" in {
        for (pg <- fixtures.values) {
          val covered: Polygon = OrthogonalPolygonBuilder.cover(pg)

          val randomPolygon: Polygon = generatePolygon()
          val randomCover: Polygon = OrthogonalPolygonBuilder.cover(randomPolygon)

          covered covers pg should be(true)
          randomCover covers randomPolygon should be(true)
        }
      }
    }

    "createInteriorCover" should {
      "return polygons that lay within the original polygon" in {
        for ((name, polygon) <- fixtures) {
          val interior: List[Polygon] = OrthogonalPolygonBuilder
            .createInteriorCover(polygon)

          val exterior: Polygon = createPolygon(polygon.getExteriorRing.getCoordinates)

          for (pg <- interior)
            exterior contains pg should be (true)
        }
      }
    }
  }
}

