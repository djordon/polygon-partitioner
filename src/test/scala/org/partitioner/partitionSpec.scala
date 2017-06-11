package org.partitioner

import org.scalatest.{Matchers, WordSpec}
import com.vividsolutions.jts.geom.{Polygon, Coordinate, Geometry}
import com.vividsolutions.jts.operation.union.CascadedPolygonUnion

import scala.collection.JavaConverters._
import scala.language.reflectiveCalls


class PolygonPartitionSpec extends WordSpec with Matchers with PolygonFixtures {
  import GeometryUtils.IterablePolygon

  def rectangles2Polygon(recs: List[Rectangle]): Polygon = {
    val envelopes: List[Geometry] = recs.map { r =>
      List(new Coordinate(r.upperLeft.x, r.upperLeft.y),
           new Coordinate(r.lowerRight.x, r.lowerRight.y))
    }
      .map(OrthogonalPolygonBuilder.coverCoordinates(_))
//    println(recs)
//    println(envelopes)
    val union: Polygon = CascadedPolygonUnion
      .union(envelopes.asJavaCollection)
      .asInstanceOf[Polygon]

    PolygonApproximator.removeAxisAlignedColinearity(union)
  }

  "OrthogonalPolygonCornerExtender" can {
    "extendCorners" should {
      "extend the corners until they hit the boundary" in {
        val corners: List[Corner] = OrthogonalPolygonPartitioner
          .extractCorners(fixtures.approximatedPolygon).head

        val startsVertically: Boolean = corners.head.angle.abs != 90
        val vc: List[Corner] = if (startsVertically) corners.init else corners.tail

        val vEdges: Set[ExtendedCorner] = OrthogonalPolygonCornerExtender
          .extendCorners(vc)(extendVertically=false).map(_.asInstanceOf[ExtendedCorner]).toSet

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
          .extractCorners(fixtures.approximatedPolygon).head

        val points: Iterable[Point] = fixtures.approximatedPolygon
          .map(c => Point(c.x, c.y))

        corners.map(_.point).toSet shouldEqual points.toSet
      }

      "calculate predictable angles for the corners" in {
        val corners: List[Corner] = OrthogonalPolygonPartitioner
          .extractCorners(fixtures.approximatedPolygon).head

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

      "handle polygons with chords" in {
        val polygons: List[Polygon] = List(
          fixtures.complexChordedPolygon1,
          fixtures.complexChordedPolygon2,
          fixtures.complexChordedPolygon3
        )
        val partitions = polygons
            .map(OrthogonalPolygonPartitioner.partition)
            .map(rectangles2Polygon)

        for (partitionPolygon <- partitions zip polygons)
          partitionPolygon._1 shouldEqual partitionPolygon._2
      }
    }
  }

  "OrthogonalPolygonDecomposer" can {
    import OrthogonalPolygonDecomposer.decompose

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
      import OrthogonalPolygonCornerExtender.extendCorners

      "extract chords aligned with specified axis" in {
        val corners: List[Corner] = extractCorners(fixtures.chordedPolygon).head
        val startsVertically: Boolean = corners.head.angle.abs != 90

        val hc: List[Corner] = if (startsVertically) corners.tail else corners.init
        val vc: List[Corner] = if (startsVertically) corners.init else corners.tail

        val vChords: List[Chord] = extendCorners(hc)(true)
          .filter(_.isInstanceOf[Chord]).asInstanceOf[List[Chord]]

        val hChords: List[Chord] = extendCorners(vc)(false)
          .filter(_.isInstanceOf[Chord]).asInstanceOf[List[Chord]]
  
        vChords.head.source.point shouldEqual Point(2.0, 1.0)
        vChords.head.dest.point shouldEqual Point(2.0, 0.0)

        hChords.head.source.point shouldEqual Point(2.0, 0.0)
        hChords.head.dest.point shouldEqual Point(1.0, 0.0)

        vChords.length shouldEqual 1
        hChords.length shouldEqual 1
      }

      "only extract choords that don't cross" in {
        val chords = extractChords(fixtures.complexChordedPolygon1)
        chords.length shouldEqual 2

        val corners: List[Corner] = extractCorners(fixtures.complexChordedPolygon1).head
        val startsVertically: Boolean = corners.head.angle.abs != 90

        val hc: List[Corner] = if (startsVertically) corners.tail else corners.init
        val vc: List[Corner] = if (startsVertically) corners.init else corners.tail

        val vChords: List[Chord] = extendCorners(hc)(true)
          .filter(_.isInstanceOf[Chord]).asInstanceOf[List[Chord]]

        val hChords: List[Chord] = extendCorners(vc)(false)
          .filter(_.isInstanceOf[Chord]).asInstanceOf[List[Chord]]


        vChords.length shouldEqual 2
        hChords.length shouldEqual 1

        hChords.head shouldEqual Chord(Corner(Point(2.0, 0.0), true, 180), Corner(Point(1.0, 0.0), true, 90))
        chords.contains(hChords.head) should be (false)
        vChords shouldEqual chords
      }

      "extract choords when there are no crossings" in {
        val chords = extractChords(fixtures.complexChordedPolygon3)

        chords.length shouldEqual 4

        val corners: List[Corner] = extractCorners(fixtures.complexChordedPolygon3).head
        val startsVertically: Boolean = corners.head.angle.abs != 90

        val hc: List[Corner] = if (startsVertically) corners.tail else corners.init
        val vc: List[Corner] = if (startsVertically) corners.init else corners.tail

        val vChords: List[Chord] = extendCorners(hc)(true)
          .filter(_.isInstanceOf[Chord]).asInstanceOf[List[Chord]]

        val hChords: List[Chord] = extendCorners(vc)(false)
          .filter(_.isInstanceOf[Chord]).asInstanceOf[List[Chord]]
  
        vChords.length shouldEqual 2
        hChords.length shouldEqual 2

        val expectedHorizontalChords: Set[Chord] = Set(
          Chord(Corner(Point(3.0, 1.0), true, 180), Corner(Point(2.0, 1.0), true, 90)),
          Chord(Corner(Point(4.0, 6.0), true, 0), Corner(Point(5.0, 6.0), true, -90))
        )
        val expectedVerticalChords: Set[Chord] = Set(
          Chord(Corner(Point(1.0, 2.0), true, 90), Corner(Point(1.0, 3.0), true, 0)),
          Chord(Corner(Point(6.0, 5.0), true, -90), Corner(Point(6.0, 4.0), true, 180))
        )
        hChords.toSet shouldEqual expectedHorizontalChords
        vChords.toSet shouldEqual expectedVerticalChords
      }
    }
  }
}
