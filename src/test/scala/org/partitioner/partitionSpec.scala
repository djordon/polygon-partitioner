package org.partitioner

import org.scalatest.{Matchers, WordSpec}
import com.vividsolutions.jts.geom.{Coordinate, Geometry, Polygon}
import com.vividsolutions.jts.operation.union.CascadedPolygonUnion
import org.partitioner.partition._

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

    val union: Polygon = CascadedPolygonUnion
      .union(envelopes.asJavaCollection)
      .asInstanceOf[Polygon]

    PolygonApproximator.removeAxisAlignedColinearity(union)
  }

  "OrthogonalPolygonPartitioner" can {
    "extractCorners" should {
      "create a corner for each coordinate in a polygon" in {
        val corners: List[Corner] = CornerExtractor
          .extractCorners(fixtures.approximatedPolygon).head

        val points: Iterable[Point] = fixtures.approximatedPolygon
          .map(c => Point(c.x, c.y))

        corners.map(_.point).toSet shouldEqual points.toSet
      }

      "calculate predictable angles for the corners" in {
        val corners: List[Corner] = CornerExtractor
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

      "handle have very few partitions" in {
        val polygons: List[Polygon] = List(
          fixtures.complexChordedPolygon1,
          fixtures.complexChordedPolygon2,
          fixtures.complexChordedPolygon3
        )
        val partitions: List[List[Rectangle]] = polygons
          .map(OrthogonalPolygonPartitioner.partition)

        partitions(0).length shouldEqual 4
        partitions(1).length shouldEqual 5
        partitions(2).length shouldEqual 5
      }

      "create rectangles that do not overlap with one another" in {
        def testOverlap(polys: List[Polygon]): Seq[Boolean] = {
          for {
            i <- 0 until polys.length - 1
            j <- i + 1 until polys.length
          } yield polys(i).overlaps(polys(j))
        }

        val partitions: List[List[Polygon]] = fixtures.allPolygons
          .map(OrthogonalPolygonPartitioner.partition)
          .map(_.map(r => r.toPolygon))

        val overlaps: List[Boolean] = partitions
          .map(testOverlap)
          .filter(_.length > 0)
          .map(se => se.reduce(_ || _))

        overlaps.reduce(_ || _) should be (false)
      }
    }
  }
}

class CornerExtenderSpec extends WordSpec with Matchers with PolygonFixtures {

  "OrthogonalPolygonCornerExtender" can {
    "extendCorners" should {
      "extend the corners until they hit the boundary" in {
        val corners: List[Corner] = CornerExtractor
          .extractCorners(fixtures.approximatedPolygon).head

        val startsVertically: Boolean = corners.head.angle.abs != 90
        val vc: List[Corner] = if (startsVertically) corners.init else corners.tail

        val vEdges: Set[CornerLine] = OrthogonalPolygonCornerExtender
          .extendCorners(vc)(extendVertically = false).map(_.asInstanceOf[CornerLine]).toSet

        val expectedEdges: Set[CornerLine] = Set(
          CornerLine(Point(0.25, 1.5), Point(1.0, 1.5), 0),
          CornerLine(Point(0.5, 1.75), Point(1.0, 1.75), 0)
        )
        vEdges shouldEqual expectedEdges
      }
    }
  }
}

class ChordReducerSpec extends WordSpec with Matchers with PolygonFixtures {
  import OrthogonalPolygonChordReducer.{reduceChords, computeIntersections}
  import OrthogonalPolygonPartitioner.extractInternalEdges
  import CornerExtractor.extractCorners

  "OrthogonalPolygonChordReducer" can {
    "computeIntersections" should {
      "find all intersection between horizontal and vertical chords" in {
        val corners: List[List[Corner]] = extractCorners(fixtures.complexChordedPolygon1)
        val vEdges: List[CornerGeometry] = extractInternalEdges(corners, true)
        val hEdges: List[CornerGeometry] = extractInternalEdges(corners, false)

        val allChords: List[Chord] = (vEdges ::: hEdges) collect { case c: Chord => c }
        val chords: List[(Chord, Chord)] = computeIntersections(allChords)

        allChords.length shouldEqual 3
        chords.length shouldEqual 2
      }

      "find chords that intersect" in {
        val corners: List[List[Corner]] = extractCorners(fixtures.complexChordedPolygon1)
        val vEdges: List[CornerGeometry] = extractInternalEdges(corners, true)
        val hEdges: List[CornerGeometry] = extractInternalEdges(corners, false)

        val allChords: List[Chord] = (vEdges ::: hEdges) collect { case c: Chord => c }
        val chords: List[(Chord, Chord)] = computeIntersections(allChords)

        def chordsIntersect(a: Chord, b: Chord): Boolean = {
          (a.yMin <= b.y && b.y <= a.yMax) && (b.xMin <= a.x && a.x <= b.xMax)
        }

        val intersects: List[Boolean] = chords.map { case (ch1, ch2) =>
          ch1.angle.abs match {
            case 90 => chordsIntersect(ch1, ch2)
            case _ => chordsIntersect(ch2, ch1)
          }
        }

        intersects.reduce(_ && _) should be (true)
      }
    }

    "reduceChords" should {
      "return a subset of non-intersecting chords" in {
        val corners: List[List[Corner]] = extractCorners(fixtures.complexChordedPolygon1)
        val vEdges: List[CornerGeometry] = extractInternalEdges(corners, true)
        val hEdges: List[CornerGeometry] = extractInternalEdges(corners, false)

        val allChords: List[Chord] = (vEdges ::: hEdges) collect { case c: Chord => c }
        val intersecting: List[(Chord, Chord)] = computeIntersections(allChords)
        val intersectingMap = intersecting.toMap ++ intersecting.map(_.swap).toMap
        val chords: List[Chord] = reduceChords(allChords)

        allChords.length shouldEqual 3
        chords.length shouldEqual 2

        intersectingMap(chords.head) should not equal chords.last
        intersectingMap(chords.last) should not equal chords.head
      }
    }
  }
}
