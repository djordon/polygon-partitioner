package org.jeom

import org.scalatest.{Matchers, WordSpec}
import com.vividsolutions.jts.densify.Densifier
import com.vividsolutions.jts.geom.{Polygon, GeometryFactory}
import com.vividsolutions.jts.io.WKTReader
import com.vividsolutions.jts.shape.random.RandomPointsBuilder

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
      .read("""
        |Polygon ((0 0, 0 1, 0.25 1.25, 0.5 1.5,
        | 0.75 1.75, 1 2, 1 0, 0.75 0, 0.75 0.25,
        | 0.5 0.25, 0.5 0, 0 0))""".stripMargin.replaceAll("\n", " ")
      )
      .asInstanceOf[Polygon]
//"Polygon ((0 0, 0 1, 0.25 1.25, 0.5 1.5, 0.75 1.75, 1 2, 1 0, 0.75 0, 0.75 0.25, 0.5 0.25, 0.5 0, 0 0))")
    val approximatedPolygon: Polygon = wktReader
      .read("Polygon ((0 0, 0 1.5, 0.25 1.5, 0.25 1.75, 0.5 1.75, 0.5 2, 1 2, 1 0, 0 0))")
      .asInstanceOf[Polygon]
  }
}


class PolygonApproximationSpec extends WordSpec with Matchers with PolygonFixtures {
  val geometryFactory = new GeometryFactory()
  val randomGeometryFactory = new RandomPointsBuilder()

  def polygonGenerator(numPoints: Int = 50): Polygon = {
    randomGeometryFactory.setNumPoints(numPoints)

    randomGeometryFactory
      .getGeometry
      .convexHull
      .asInstanceOf[Polygon]
  }

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
    "cover" should {
      "work as create the expected polygon" in {
        val covered: Polygon = OrthogonalPolygonBuilder
          .cover(fixtures.preDensifiedPolygon)

        covered shouldEqual (fixtures.approximatedPolygon)
      }

      "create an orthogonal polygon" in {
        val randomPolygon: Polygon = polygonGenerator()
        val randomCover: Polygon = OrthogonalPolygonBuilder.cover(randomPolygon)
        val axisAlignedAngles: Set[Double] = Set(0.0, 90.0, 180.0, -90.0, 270.0)

        val vecs: List[Vec] = PolygonSimplifier.polygon2vecs(randomCover)
        val isAxisAligned: List[Boolean] = vecs
          .map(v => axisAlignedAngles.contains(v.angle))
  
        isAxisAligned.reduce(_ && _) should be (true)
      }

      "cover the polygon" in {
        val covered: Polygon = OrthogonalPolygonBuilder
          .cover(fixtures.preDensifiedPolygon)

        val randomPolygon: Polygon = polygonGenerator()
        val randomCover: Polygon = OrthogonalPolygonBuilder.cover(randomPolygon)

        covered covers fixtures.preDensifiedPolygon should be (true)
        randomCover covers randomPolygon should be (true)
      }
    }

    "build" should {
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
