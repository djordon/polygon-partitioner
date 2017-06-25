package org.partitioner

import org.scalatest.{Matchers, WordSpec}
import com.vividsolutions.jts.geom.{Coordinate, Geometry, Polygon}
import com.vividsolutions.jts.operation.union.CascadedPolygonUnion
import org.partitioner.GeometryUtils.normalizePolygon
import org.partitioner.partition._

import scala.collection.JavaConverters._
import scala.language.reflectiveCalls


class PolygonPartitionSpec extends WordSpec with Matchers with PolygonFixtures {
  import GeometryUtils.{IterablePolygon, normalizePolygon}

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
    "createChordsCornerLines" should {
      import OrthogonalPolygonPartitioner.createChordsCornerLines

      "create a list of CornerLines whose source isn't an endpoint of a chord" in {
        for (pg <- orthogonalPolygonFixtures.values) {
          val polygon: Polygon = normalizePolygon(pg)
          val corners: List[List[Corner]] = CornerExtractor.extractCorners(polygon)

          val (chords, lines) = createChordsCornerLines(corners)

          val chordEndpoints: Set[Point] = chords
            .flatMap(_.toListCorner)
            .map(_.point)
            .toSet

          val expectedValues: Set[Set[Boolean]] = Set(Set(), Set(false))

          expectedValues should contain {
            lines.map(cl => chordEndpoints.contains(cl.source)).toSet
          }
        }
      }
    }
    "extractCorners" should {
      "create a corner for each coordinate in a polygon" in {
        val corners: List[Corner] = CornerExtractor
          .extractCorners(fixtures("approximatedPolygon")).head

        val points: Iterable[Point] = fixtures("approximatedPolygon")
          .map(c => Point(c.x, c.y))

        corners.map(_.point).toSet shouldEqual points.toSet
      }

      "calculate predictable angles for the corners" in {
        val corners: List[Corner] = CornerExtractor
          .extractCorners(fixtures("approximatedPolygon")).head

        val angles: List[Int] = List(0, 90, 0, 90, 0, -90, 180, 90, 0)

        corners.map(_.angle) shouldEqual angles
      }
    }

    "partition" should {
      "extract non-degenerate rectangles from an orthogonal polygon" in {
        val poly1: Polygon = fixtures("approximatedPolygon")
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
        val polygons: List[Polygon] = orthogonalPolygonFixtures.toList.map(_._2)

        val rectangles: List[List[Polygon]] = polygons
          .map { OrthogonalPolygonPartitioner.partition }
          .map { _.map(rec => rec.toPolygon) }

        val reconstructedPolygons: List[Polygon] = rectangles
          .map { geos => CascadedPolygonUnion.union(geos.asJavaCollection) }
          .map { _.asInstanceOf[Polygon] }
          .map { PolygonApproximator.removeAxisAlignedColinearity }

        for (polygonPairs <- reconstructedPolygons zip polygons.map(normalizePolygon))
          polygonPairs._1 shouldEqual polygonPairs._2
      }

      "handle polygons with chords" in {
        val polygons: List[Polygon] = List(
          "chordedPolygon1",
          "chordedPolygon2",
          "chordedPolygon3"
        ).map(fixtures(_))

        val partitions = polygons
            .map(OrthogonalPolygonPartitioner.partition)
            .map(rectangles2Polygon)

        for (partitionPolygon <- partitions zip polygons)
          partitionPolygon._1 shouldEqual partitionPolygon._2
      }

      "handle have very few partitions" in {
        val polygons: List[Polygon] = List(
          "chordedPolygon1",
          "chordedPolygon2",
          "chordedPolygon3"
        ).map(fixtures(_))

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

        val partitions: List[List[Polygon]] = fixtures.values
          .map(OrthogonalPolygonPartitioner.partition)
          .map(_.map(r => r.toPolygon))
          .toList

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
  import OrthogonalPolygonCornerExtender.extendCorners

  "OrthogonalPolygonCornerExtender" can {
    "extendCorners" should {
      "extend the corners until they hit the boundary" in {
        val corners: List[Corner] = CornerExtractor
          .extractCorners(fixtures("approximatedPolygon")).head

        val startsVertically: Boolean = corners.head.angle.abs != 90
        val vc: List[Corner] = if (startsVertically) corners.init else corners.tail

        val vEdges: Set[CornerLine] = extendCorners(vc)(extendVertically = false)
          .map(_.asInstanceOf[CornerLine]).toSet

        val expectedEdges: Set[CornerLine] = Set(
          CornerLine(Point(0.25, 1.5), Point(1.0, 1.5), 0),
          CornerLine(Point(0.5, 1.75), Point(1.0, 1.75), 0)
        )
        vEdges shouldEqual expectedEdges
      }

      "create duplicate chords when the corners point in the same direction" in {
        for (polygonName <- List("complexPolygon0", "chordedPolygon1")) {
          val polygon: Polygon = normalizePolygon(orthogonalPolygonFixtures(polygonName))
          val corners: List[List[Corner]] = CornerExtractor.extractCorners(polygon)

          val extended = extendCorners(corners.flatMap(_.tail))(true)
              .collect { case ch: Chord => ch }

          val sameChords: Set[Chord] = extended
            .map(_.swap)
            .toSet
            .intersect(extended.toSet)

          sameChords should not be empty

          sameChords.map { ch =>
            ch.source.angle == ch.dest.oppositeAngle
          } should be (Set(true))
        }
      }
    }
  }
}

class CornerExtractorSpec extends WordSpec with Matchers with PolygonFixtures {
  import GeometryUtils.{IterablePolygon, normalizePolygon}

  "CornerExtractor" can {
    "makeCorners" should {
      "create a corner for each of the vertices on boundary of a polygon" in {
        val polygons: List[Polygon] = orthogonalPolygonFixtures.toList.map(_._2)
        val corners: List[List[Corner]] = polygons
          .map(_.toList.tail)
          .map(CornerExtractor.makeCorners)

        corners.map(_.length) shouldEqual polygons.map(_.getExteriorRing.getNumPoints)
      }

      "the first two corner have angles of 90 and 0 degrees respectively for normalized polygons" in {
        val polygons: List[Polygon] = orthogonalPolygonFixtures.toList.map(_._2)
        val cornersInit: List[List[Corner]] = polygons
          .map(normalizePolygon)
          .map(_.toList.init)
          .map(CornerExtractor.makeCorners)

        cornersInit.map(_.head.angle == 90).reduce(_ && _) should be (true)

        val cornersTail: List[List[Corner]] = polygons
          .map(normalizePolygon)
          .map(_.toList.tail)
          .map(CornerExtractor.makeCorners)

        cornersTail.map(_.head.angle == 0).reduce(_ && _) should be (true)
      }
    }

    "extractCorners" should {
      "Create a corner for each vertex, including vertices on the holes" in {
        val polygons: List[Polygon] = orthogonalPolygonFixtures.toList.map(_._2)
        val corners: List[List[Corner]] = polygons
          .map(CornerExtractor.extractCorners(_).flatten)

        corners.map(_.length) shouldEqual polygons.map(_.getNumPoints)
      }

      "create a list where the first corner and the last corner are the sane" in {
        val polygons: List[Polygon] = orthogonalPolygonFixtures.toList.map(_._2)
        val matched: List[Boolean] = polygons
          .map(CornerExtractor.extractCorners(_))
          .flatMap(_.map(cns => cns.head == cns.last))

        matched.reduce(_ && _) should be(true)
      }
    }
  }
}


class ChordReducerSpec extends WordSpec with Matchers with PolygonFixtures {
  import OrthogonalPolygonChordReducer.{reduceChords, computeIntersections, removeDuplicateChords}
  import OrthogonalPolygonPartitioner.createInteriorLines
  import CornerExtractor.extractCorners

  "OrthogonalPolygonChordReducer" can {
    "computeIntersections" should {
      "find all intersection between horizontal and vertical chords" in {
        val corners: List[List[Corner]] = extractCorners(fixtures("chordedPolygon1"))
        val interiorLines: List[CornerGeometry] = createInteriorLines(corners)

        val allChords: List[Chord] = interiorLines collect { case c: Chord => c }
        val uniqueChords: List[Chord] = removeDuplicateChords(allChords)
        val chords: List[(Chord, Chord)] = computeIntersections(uniqueChords)

        uniqueChords.length shouldEqual 3
        chords.length shouldEqual 2
      }

      "find chords that intersect" in {
        val corners: List[List[Corner]] = extractCorners(fixtures("chordedPolygon1"))
        val interiorLines: List[CornerGeometry] = createInteriorLines(corners)

        val allChords: List[Chord] = interiorLines collect { case c: Chord => c }
        val uniqueChords: List[Chord] = removeDuplicateChords(allChords)
        val chords: List[(Chord, Chord)] = computeIntersections(uniqueChords)

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
        val corners: List[List[Corner]] = extractCorners(fixtures("chordedPolygon1"))
        val interiorLines: List[CornerGeometry] = createInteriorLines(corners)

        val allChords: List[Chord] = interiorLines collect { case c: Chord => c }
        val uniqueChords: List[Chord] = removeDuplicateChords(allChords)
        val intersecting: List[(Chord, Chord)] = computeIntersections(uniqueChords)
        val intersectingMap = intersecting.toMap ++ intersecting.map(_.swap).toMap
        val chords: List[Chord] = reduceChords(allChords)

        uniqueChords.length shouldEqual 3
        chords.length shouldEqual 2

        intersectingMap(chords.head) should not equal chords.last
        intersectingMap(chords.last) should not equal chords.head
      }
    }

    "removeDuplicateChords" should {
      "remove chords that have the same endpoints, but in reverse" in {
        val corners: List[List[Corner]] = extractCorners(fixtures("chordedPolygon1"))
        val interiorLines: List[CornerGeometry] = createInteriorLines(corners)

        val allChords: List[Chord] = interiorLines collect { case c: Chord => c }
        val uniqueChords: List[Chord] = removeDuplicateChords(allChords)

        uniqueChords.distinct shouldEqual uniqueChords
        uniqueChords.map(_.swap).toSet.intersect(uniqueChords.toSet) should be (Set[Chord]())
      }

      "have fewer chords in cases where two concave corners point at each other" in {
        for (polygonName <- List("chordedPolygon1", "complexPolygon0")) {
          val corners: List[List[Corner]] = extractCorners(fixtures(polygonName))
          val interiorLines: List[CornerGeometry] = createInteriorLines(corners)

          val allChords: List[Chord] = interiorLines collect { case c: Chord => c }
          val uniqueChords: List[Chord] = removeDuplicateChords(allChords)

          allChords.length should be > uniqueChords.length
        }
      }
    }
  }
}
