package org.jeom

import org.scalatest.{Matchers, WordSpec}
import com.vividsolutions.jts.densify.Densifier
import com.vividsolutions.jts.geom.{Polygon, GeometryFactory}
import com.vividsolutions.jts.io.WKTReader

import com.vividsolutions.jts.operation.union.CascadedPolygonUnion

import scala.collection.JavaConverters._
import scala.language.reflectiveCalls


trait PolygonFixtures {
  val wktReader = new WKTReader()

  val fixtures = new {
    val simplePolygon: Polygon = wktReader
      .read("Polygon ((0 0, 0 1, 1 2, 1 0, 0 0))")
      .asInstanceOf[Polygon]

    val lessRedundentPointPolygon: Polygon = wktReader
      .read("Polygon ((0 0, 0 1, 0.5 1.5, 1 2, 1 0, 0 0))")
      .asInstanceOf[Polygon]

    val redundentBasisPointPolygon: Polygon = wktReader
      .read("Polygon ((0 0, 0 1, 1 2, 1 1, 1 0, 0 0))")
      .asInstanceOf[Polygon]

    val redundentPointPolygon: Polygon = wktReader
      .read("Polygon ((0 0, 0 1, 0.5 1.5, 1 2, 1 1, 1 0, 0 0))")
      .asInstanceOf[Polygon]

    val preDensifiedPolygon: Polygon = wktReader
      .read("Polygon ((0 0, 0 1, 0.25 1.25, 0.5 1.5, 0.75 1.75, 1 2, 1 0, 0.75 0, 0.75 0.25, 0.5 0.25, 0.5 0, 0 0))")
      .asInstanceOf[Polygon]

    val approximatedPolygon: Polygon = wktReader
      .read("Polygon ((0 0, 0 1.5, 0.25 1.5, 0.25 1.75, 0.5 1.75, 0.5 2, 1 2, 1 0, 0 0))")
      .asInstanceOf[Polygon]
  }
}


class PolygonSimplifierSpec extends WordSpec with Matchers with PolygonFixtures {
  val geometryFactory = new GeometryFactory()

  "PolygonSimplifier" can {

    "removeAxisAlignedColinearity" should {
      "remove redundent points that lie on axis aligned lines" in {
        val simplified: Polygon = PolygonSimplifier
          .removeAxisAlignedColinearity(fixtures.redundentBasisPointPolygon)

        simplified shouldEqual fixtures.simplePolygon
      }

      "keep redundent points that lie on non-axis aligned lines" in {
        val simplified: Polygon = PolygonSimplifier
          .removeAxisAlignedColinearity(fixtures.redundentPointPolygon)

        val nothingHappened: Polygon = PolygonSimplifier
          .removeAxisAlignedColinearity(fixtures.preDensifiedPolygon)

        simplified shouldEqual fixtures.lessRedundentPointPolygon
        nothingHappened shouldEqual fixtures.preDensifiedPolygon
      }
    }

    "removeColinearity" should {
      "remove all colinear points" in {
        val simplified: Polygon = PolygonSimplifier
          .removeColinearity(fixtures.redundentPointPolygon)

        simplified shouldEqual fixtures.simplePolygon
      }
    }
  }

  "OrthogonalPolygonBuilder" can {
    "build" should {
      "not modify input polygon when tolerance is zero" in {
        val approximated1: Polygon = OrthogonalPolygonBuilder
          .build(fixtures.preDensifiedPolygon, tolerance=0, densify=true)

        val approximated2: Polygon = OrthogonalPolygonBuilder
          .build(fixtures.preDensifiedPolygon, tolerance=0, densify=false)

        approximated1 shouldEqual fixtures.approximatedPolygon
        approximated2 shouldEqual fixtures.approximatedPolygon
      }

      "densifying should lead to better approximations" in {
        val approximated1: Polygon = OrthogonalPolygonBuilder
          .build(fixtures.preDensifiedPolygon, tolerance=0.1, densify=true)

        val approximated2: Polygon = OrthogonalPolygonBuilder
          .build(fixtures.preDensifiedPolygon, tolerance=0.01, densify=false)

        val diffArea1: Double = approximated1
          .difference(fixtures.preDensifiedPolygon)
          .getArea

        val diffArea2: Double = approximated2
          .difference(fixtures.preDensifiedPolygon)
          .getArea

        diffArea1 should be < diffArea2
      }
    }
  }
}
