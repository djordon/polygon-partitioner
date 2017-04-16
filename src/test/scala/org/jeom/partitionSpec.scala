package org.jeom

import org.scalatest.{Matchers, WordSpec}
import org.scalactic.TolerantNumerics
import com.vividsolutions.jts.geom.{Polygon, Coordinate, Geometry, LineString}
import com.vividsolutions.jts.io.WKTReader
import com.vividsolutions.jts.operation.union.CascadedPolygonUnion

import scala.collection.JavaConverters._
import scala.language.reflectiveCalls


class PolygonPartitionSpec extends WordSpec with Matchers with PolygonFixtures {
  import GeometryUtils.IterablePolygon

  "OrthononalPolygonCornerExtender" can {
    "extendCorners" should {
      "extend the corners until they hit the boundary" in {
        val corners: List[Corner] = OrthogonalPolygonPartitioner
          .extractCorners(fixtures.approximatedPolygon)

        val startsVertically: Boolean = corners.head.angle.abs != 90
        val vc: List[Corner] = if (startsVertically) corners.init else corners.tail

        val vEdges: Set[ExtendedCorner] = OrthononalPolygonCornerExtender
          .extendCorners(vc, extendVertically=false).toSet

        val expectedEdges: Set[ExtendedCorner] = Set(
          ExtendedCorner(Point(0.25, 1.5), Point(1.0, 1.5), 0),
          ExtendedCorner(Point(0.5, 1.75), Point(1.0, 1.75), 0)
        )
        vEdges shouldEqual expectedEdges
      }
    }
  }

  "OrthogonalPolygonPartitioner" can {
    "extractCorners" should {
      "create a corner for each coordinate in a polygon" in {
        val corners: List[Corner] = OrthogonalPolygonPartitioner
          .extractCorners(fixtures.approximatedPolygon)

        val points: Iterable[Point] = fixtures.approximatedPolygon
          .map(c => Point(c.x, c.y))

        corners.map(_.point).toSet shouldEqual points.toSet
      }

      "calculate predictable angles for the corners" in {
        val corners: List[Corner] = OrthogonalPolygonPartitioner
          .extractCorners(fixtures.approximatedPolygon)

        val angles: List[Int] = List(0, 90, 0, 90, 0, -90, 180, 90, 0)

        corners.map(_.angle) shouldEqual angles
      }
    }

    "partition" should {
      "extract non-degenerate rectangles from an orthogonal polygon" in {
        val poly1: Polygon = fixtures.approximatedPolygon
          .norm()
          .asInstanceOf[Polygon]

        val recs1: List[Rectangle] = OrthogonalPolygonPartitioner
          .partition(poly1)

        def isRectangle(rec: Rectangle): Boolean = {
          (rec.upperLeft.x != rec.lowerRight.x) &&
          (rec.upperLeft.y != rec.lowerRight.y)
        }

        recs1.map(isRectangle).reduce(_ && _) should be (true)

        val poly2: Polygon = OrthogonalPolygonBuilder.cover(generatePolygon())
        val recs2: List[Rectangle] = OrthogonalPolygonPartitioner.partition(poly2)

        recs2.map(isRectangle).reduce(_ && _) should be (true)
      }

      "create rectangles that union to the original polygon" in {
        val poly: Polygon = fixtures.approximatedPolygon
          .norm()
          .asInstanceOf[Polygon]

        val recs: List[Rectangle] = OrthogonalPolygonPartitioner
          .partition(poly)

        val envs: List[Geometry] = recs.map { r => 
          List(new Coordinate(r.upperLeft.x, r.upperLeft.y),
               new Coordinate(r.lowerRight.x, r.lowerRight.y))
        }
        .map(OrthogonalPolygonBuilder.coverCoordinates(_))

        val union: Polygon = CascadedPolygonUnion
          .union(envs.asJavaCollection)
          .asInstanceOf[Polygon]

        val simplifiedPolygon: Polygon = PolygonApproximator
          .removeAxisAlignedColinearity(union)

        simplifiedPolygon shouldEqual poly
      }
    }
  }

  "OrthogonalPolygonDecomposer" can {
    import OrthogonalPolygonDecomposer.decompose
      import OrthogonalPolygonDecomposer.extractChords
      import OrthogonalPolygonPartitioner.extractCorners
    "decompose" should {

      "leave chordless polygons alone" in {
        val polygons: List[Polygon] = decompose(fixtures.simplePolygon)

        polygons.head shouldEqual fixtures.simplePolygon
        polygons.length shouldEqual 1
      }

      "break down chorded polygons to chordless polygons" in {
        val polygons: List[Polygon] = decompose(fixtures.chordedPolygon)
        val chordless: List[Polygon] = polygons.flatMap(decompose(_))

        polygons shouldEqual chordless
      }

      "create polygons that union to the original" in {
        val polygons: List[Polygon] = decompose(fixtures.chordedPolygon)

        val union: Polygon = CascadedPolygonUnion
          .union(polygons.asJavaCollection)
          .asInstanceOf[Polygon]

        val unifiedPolygon: Polygon = PolygonApproximator
          .removeAxisAlignedColinearity(union)

        unifiedPolygon shouldEqual fixtures.chordedPolygon
      }
    }

    "extractChords" should {
      import OrthogonalPolygonDecomposer.extractChords
      import OrthogonalPolygonPartitioner.extractCorners

      "extract chords aligned with specified axis" in {
        val corners: List[Corner] = extractCorners(fixtures.chordedPolygon)
        val startsVertically: Boolean = corners.head.angle.abs != 90

        val hc: List[Corner] = if (startsVertically) corners.tail else corners.init
        val vc: List[Corner] = if (startsVertically) corners.init else corners.tail

        val vChords: List[ExtendedCorner] = extractChords(hc, true)
        val hChords: List[ExtendedCorner] = extractChords(vc, false)
  
        vChords.head.source shouldEqual Point(2.0, 1.0)
        vChords.head.dest shouldEqual Point(2.0, 0.0)

        hChords.head.source shouldEqual Point(2.0, 0.0)
        hChords.head.dest shouldEqual Point(1.0, 0.0)

        vChords.length shouldEqual 1
        hChords.length shouldEqual 1
      }

      "only extract choords that don't cross" in {
        val chords = extractChords(fixtures.complexChordedPolygon)

        chords.length shouldEqual 2

        val corners: List[Corner] = extractCorners(fixtures.complexChordedPolygon)
        val startsVertically: Boolean = corners.head.angle.abs != 90

        val hc: List[Corner] = if (startsVertically) corners.tail else corners.init
        val vc: List[Corner] = if (startsVertically) corners.init else corners.tail

        val vChords: List[ExtendedCorner] = extractChords(hc, true)
        val hChords: List[ExtendedCorner] = extractChords(vc, false)
  
        vChords.length shouldEqual 2
        hChords.length shouldEqual 1

        hChords.head shouldEqual ExtendedCorner(Point(2.0, 0.0), Point(1.0, 0.0), 180)
        chords.contains(hChords.head) should be (false)
        vChords shouldEqual chords
      }
    }
  }
}
