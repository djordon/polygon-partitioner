package org.partitioner

import org.scalatest.{Matchers, WordSpec}
import org.scalactic.TolerantNumerics

import com.vividsolutions.jts.geom.Polygon


class PolygonApproximationSpec extends WordSpec with Matchers with PolygonFixtures {
  val epsilon: Double = 1.1102230246251568E-16
  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(epsilon)

  "PolygonApproximator" can {
    import GeometryUtils.normalizePolygon

    "removeAxisAlignedColinearity" should {
      "remove redundent points that lie on axis aligned lines" in {
        val simplified: Polygon = PolygonApproximator
          .removeAxisAlignedColinearity(fixtures("redundantBasisPointPolygon"))

        simplified shouldEqual fixtures("simplePolygon")
      }

      "keep redundant points that lie on non-axis aligned lines" in {
        val simplified: Polygon = PolygonApproximator
          .removeAxisAlignedColinearity(fixtures("redundantPointPolygon"))

        val nothingHappened: Polygon = PolygonApproximator
          .removeAxisAlignedColinearity(fixtures("preDensifiedPolygon"))

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
    }
  }
}

class OrthogonalPolygonBuilderSpec extends WordSpec with Matchers with PolygonFixtures {
  val epsilon: Double = 1.1102230246251568E-16
  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(epsilon)

  "OrthogonalPolygonBuilder" can {
     import GeometryUtils.{IterablePolygon, createPolygon}

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

          val vecs: List[Vec] = randomCover
            .toList
            .sliding(2, 1)
            .map(Vec.apply)
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

          val vecs: List[Vec] = randomCover
            .toList
            .sliding(2, 1)
            .map(Vec.apply)
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

    "approximate" should {
      "lead to better approximations as densifyTolerance decreases" in {
        val approximated0: Polygon = OrthogonalPolygonBuilder
          .approximate(fixtures("housePolygon"), simplifyTolerance=0, densifyTolerance=0.01)

        val approximated1: Polygon = OrthogonalPolygonBuilder
          .approximate(fixtures("housePolygon"), simplifyTolerance=0, densifyTolerance=0.1)

        val approximated2: Polygon = OrthogonalPolygonBuilder
          .approximate(fixtures("housePolygon"), simplifyTolerance=0, densifyTolerance=1)

        val diffArea0: Double = approximated0
          .difference(fixtures("housePolygon"))
          .getArea

        val diffArea1: Double = approximated1
          .difference(fixtures("housePolygon"))
          .getArea

        val diffArea2: Double = approximated2
          .difference(fixtures("housePolygon"))
          .getArea

        diffArea0 should be < diffArea1
        diffArea1 should be < diffArea2
      }
    }
  }
}
