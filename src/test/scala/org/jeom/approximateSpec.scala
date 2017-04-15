package org.jeom

import org.scalatest.{Matchers, WordSpec}
import org.scalactic.TolerantNumerics
import com.vividsolutions.jts.densify.Densifier
import com.vividsolutions.jts.geom.{Polygon, GeometryFactory}
import com.vividsolutions.jts.io.WKTReader
import com.vividsolutions.jts.shape.random.RandomPointsBuilder

import scala.collection.JavaConverters._
import scala.language.reflectiveCalls


trait PolygonFixtures {
  val wktReader = new WKTReader()

  val geometryFactory = new GeometryFactory()
  val randomGeometryFactory = new RandomPointsBuilder()

  def generatePolygon(numPoints: Int = 50): Polygon = {
    randomGeometryFactory.setNumPoints(numPoints)

    randomGeometryFactory
      .getGeometry
      .convexHull
      .norm()
      .asInstanceOf[Polygon]
  }

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

    val housePolygon: Polygon = wktReader
      .read("""
        |Polygon ((0 0, 0 1, 1 2, 1 0, 0.75 0,
        | 0.75 0.25, 0.5 0.25, 0.5 0, 0 0))""".stripMargin.replaceAll("\n", " "))
      .asInstanceOf[Polygon]

    val preDensifiedPolygon: Polygon = wktReader
      .read("""
        |Polygon ((0 0, 0 1, 0.25 1.25, 0.5 1.5,
        | 0.75 1.75, 1 2, 1 0, 0.75 0, 0.75 0.25,
        | 0.5 0.25, 0.5 0, 0 0))""".stripMargin.replaceAll("\n", " "))
      .asInstanceOf[Polygon]

    val approximatedPolygon: Polygon = wktReader
      .read("""
        |Polygon ((0 0, 0 1.5, 0.25 1.5, 0.25 1.75,
        | 0.5 1.75, 0.5 2, 1 2, 1 0, 0 0))""".stripMargin.replaceAll("\n", " "))
      .asInstanceOf[Polygon]
  }
}

class PolygonApproximationSpec extends WordSpec with Matchers with PolygonFixtures {
  val epsilon: Double = 1.1102230246251568E-16
  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(epsilon)

  "PolygonApproximator" can {

    "removeAxisAlignedColinearity" should {
      "remove redundent points that lie on axis aligned lines" in {
        val simplified: Polygon = PolygonApproximator
          .removeAxisAlignedColinearity(fixtures.redundentBasisPointPolygon)

        simplified shouldEqual fixtures.simplePolygon
      }

      "keep redundent points that lie on non-axis aligned lines" in {
        val simplified: Polygon = PolygonApproximator
          .removeAxisAlignedColinearity(fixtures.redundentPointPolygon)

        val nothingHappened: Polygon = PolygonApproximator
          .removeAxisAlignedColinearity(fixtures.preDensifiedPolygon)

        simplified shouldEqual fixtures.lessRedundentPointPolygon
        nothingHappened shouldEqual fixtures.preDensifiedPolygon
      }
    }
  }

  "OrthogonalPolygonBuilder" can {
     import GeometryUtils.IterablePolygon

    "cover" should {
      "create the expected polygon" in {
        val covered: Polygon = OrthogonalPolygonBuilder
          .cover(fixtures.preDensifiedPolygon)

        covered shouldEqual (fixtures.approximatedPolygon)
      }

      "create an orthogonal polygon" in {
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
  
        isAxisAligned.reduce(_ && _) should be (true)
      }

      "cover the polygon" in {
        val covered: Polygon = OrthogonalPolygonBuilder
          .cover(fixtures.preDensifiedPolygon)

        val randomPolygon: Polygon = generatePolygon()
        val randomCover: Polygon = OrthogonalPolygonBuilder.cover(randomPolygon)

        covered covers fixtures.preDensifiedPolygon should be (true)
        randomCover covers randomPolygon should be (true)
      }
    }

    "approximate" should {
      "lead to better approximations as densifyTolerance decreases" in {
        val approximated0: Polygon = OrthogonalPolygonBuilder
          .approximate(fixtures.housePolygon, simplifyTolerance=0, densifyTolerance=0.01)

        val approximated1: Polygon = OrthogonalPolygonBuilder
          .approximate(fixtures.housePolygon, simplifyTolerance=0, densifyTolerance=0.1)

        val approximated2: Polygon = OrthogonalPolygonBuilder
          .approximate(fixtures.housePolygon, simplifyTolerance=0, densifyTolerance=1)

        val diffArea0: Double = approximated0
          .difference(fixtures.housePolygon)
          .getArea

        val diffArea1: Double = approximated1
          .difference(fixtures.housePolygon)
          .getArea

        val diffArea2: Double = approximated2
          .difference(fixtures.housePolygon)
          .getArea

        diffArea0 should be < diffArea1
        diffArea1 should be < diffArea2
      }
    }
  }
}
