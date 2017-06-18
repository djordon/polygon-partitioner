package org.partitioner

import org.scalatest.{Matchers, WordSpec}
import org.scalactic.TolerantNumerics

import com.vividsolutions.jts.geom.{Polygon, GeometryFactory}
import com.vividsolutions.jts.io.WKTReader
import com.vividsolutions.jts.shape.random.RandomPointsBuilder

import scala.language.reflectiveCalls
import scala.io.Source


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

  def loadDirectory(dir: String) : Map[String, Polygon] = Source
    .fromResource(dir)
    .getLines
    .map(f => (f, Source.fromResource(s"$dir/$f").getLines.toList.head))
    .toMap
    .mapValues(wktReader.read(_).asInstanceOf[Polygon])

  val orthogonalPolygonFixtures: Map[String, Polygon] = loadDirectory("rectilinear")
  val nonOrthogonalPolygonFixtures: Map[String, Polygon] = loadDirectory("non-rectilinear")
  val fixtures = orthogonalPolygonFixtures ++ nonOrthogonalPolygonFixtures
}

class PolygonApproximationSpec extends WordSpec with Matchers with PolygonFixtures {
  val epsilon: Double = 1.1102230246251568E-16
  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(epsilon)

  "PolygonApproximator" can {

    "removeAxisAlignedColinearity" should {
      "remove redundent points that lie on axis aligned lines" in {
        val simplified: Polygon = PolygonApproximator
          .removeAxisAlignedColinearity(fixtures("redundentBasisPointPolygon"))

        simplified shouldEqual fixtures("simplePolygon")
      }

      "keep redundent points that lie on non-axis aligned lines" in {
        val simplified: Polygon = PolygonApproximator
          .removeAxisAlignedColinearity(fixtures("redundantPointPolygon"))

        val nothingHappened: Polygon = PolygonApproximator
          .removeAxisAlignedColinearity(fixtures("preDensifiedPolygon"))

        simplified shouldEqual fixtures("lessRedundentPointPolygon")
        nothingHappened shouldEqual fixtures("preDensifiedPolygon")
      }
    }
  }

  "OrthogonalPolygonBuilder" can {
     import GeometryUtils.IterablePolygon

    "cover" should {
      "create the expected polygon" in {
        val covered: Polygon = OrthogonalPolygonBuilder
          .cover(fixtures("preDensifiedPolygon"))

        covered shouldEqual (fixtures("approximatedPolygon"))
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
          .cover(fixtures("preDensifiedPolygon"))

        val randomPolygon: Polygon = generatePolygon()
        val randomCover: Polygon = OrthogonalPolygonBuilder.cover(randomPolygon)

        covered covers fixtures("preDensifiedPolygon") should be (true)
        randomCover covers randomPolygon should be (true)
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