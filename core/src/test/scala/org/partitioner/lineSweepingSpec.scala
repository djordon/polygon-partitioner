package org.partitioner

import org.scalatest.{Matchers, WordSpec}
import com.vividsolutions.jts.geom.{Polygon, Coordinate, LineString}
import org.partitioner.partition._

import scala.collection.immutable.TreeSet
import scala.language.reflectiveCalls


class LineSweepingSpec extends WordSpec with Matchers with PolygonFixtures {

  "RectilinearLineSweeping" can {
    import CornerExtractor.extractCorners
    import CornerLineAdjuster.createLineSweepGroups
    import GeometryUtils.normalizePolygon

    "createLineSweepGroups" should {
      "create lists of geometries ordered by increasing " +
        "y-coordinates when adjusting lines horizontally" in {
        for (pg <- orthogonalPolygonFixtures.values) {
          val polygon: Polygon = normalizePolygon(pg)
          val corners: List[List[Corner]] = extractCorners(polygon)

          val (chords, lines): (List[Chord], List[CornerLine]) = {
            OrthogonalPolygonPartitioner.createChordsCornerLines(corners)
          }

          val horizontalLines: List[CornerLine] = lines.filterNot(_.pointsVertically)
          val verticalChordCorners: List[Corner] = {
            OrthogonalPolygonPartitioner.extractChordCorners(chords, vertical = true)
          }

          val groups: List[List[CornerGeometry]] = createLineSweepGroups {
            horizontalLines ::: verticalChordCorners
          }(vertical = false)

          val isIncreasing: Iterator[Boolean] = groups
            .flatMap(g => g.map(_.y))
            .sliding(2, 1)
            .map(g => g.head <= g.last)

          Set(Set(), Set(true)) should contain(isIncreasing.toSet)
        }
      }
    }

    "lineAction" should {
      "return a degenerate CornerLine if the opened lines contained the current coordinate" in {
        val c1 = Corner(Point(1, 2), true, 0)
        val c2 = Corner(Point(3, 4), true, 90)
        val c3 = Corner(Point(5, 6), true, 180)
        val c4 = Corner(Point(7, 8), true, -90)

        val cl1 = CornerLineAdjuster.lineAction(TreeSet(0, 1, 3))(c1)
        val cl2 = CornerLineAdjuster.lineAction(TreeSet(2, 4, 6))(c2)
        val cl3 = CornerLineAdjuster.lineAction(TreeSet(3, 5, 7))(c3)
        val cl4 = CornerLineAdjuster.lineAction(TreeSet(6, 8, 10))(c4)

        cl1.source shouldEqual cl1.dest
        cl2.source shouldEqual cl2.dest
        cl3.source shouldEqual cl3.dest
        cl4.source shouldEqual cl4.dest
      }

      "return a non-degenerate CornerLine if the opened lines doesn't contain the current coordinate" in {
        val c1 = Corner(Point(1, 2), true, 0)
        val c2 = Corner(Point(3, 4), true, 90)
        val c3 = Corner(Point(5, 6), true, 180)
        val c4 = Corner(Point(7, 8), true, -90)

        val cl1 = CornerLineAdjuster.lineAction(TreeSet(0, 2))(c1)
        val cl2 = CornerLineAdjuster.lineAction(TreeSet(3, 5))(c2)
        val cl3 = CornerLineAdjuster.lineAction(TreeSet(4, 6))(c3)
        val cl4 = CornerLineAdjuster.lineAction(TreeSet(7, 9))(c4)

        cl1.source should not equal cl1.dest
        cl2.source should not equal cl2.dest
        cl3.source should not equal cl3.dest
        cl4.source should not equal cl4.dest
      }
    }
  }
}


class CornerLineAdjusterSpec extends WordSpec with Matchers with PolygonFixtures {

  "CornerLineAdjuster" can {
    import CornerExtractor.extractCorners
    import CornerLineAdjuster.{adjustCornerGeometries, createLineSweepGroups, setActions}
    import GeometryUtils.normalizePolygon

    "setActions" should {
      "open all corners when given an empty opened TreeSet" in {
        for (pg <- orthogonalPolygonFixtures.values) {
          val polygon: Polygon = normalizePolygon(pg)
          val corners: List[List[Corner]] = extractCorners(polygon)

          val (chords, lines): (List[Chord], List[CornerLine]) = {
            OrthogonalPolygonPartitioner.createChordsCornerLines(corners)
          }

          val horizontalLines: List[CornerLine] = lines.filterNot(_.pointsVertically)
          val verticalChordCorners: List[Corner] = {
            OrthogonalPolygonPartitioner.extractChordCorners(chords, vertical = true)
          }

          val groups: List[List[CornerGeometry]] = createLineSweepGroups {
            horizontalLines ::: verticalChordCorners
          }(vertical = false)

          val actions = groups.map(setActions(_, TreeSet())(false))
          val pairs = actions.zip {
            groups.map { cns => cns.collect { case cn: Corner => cn } }
          }

          val openedGroups: List[Boolean] = pairs
            .filter { case (a, c) => c.length > 0 }
            .map { case (a, c) => c.map(a("toOpen").toSet.contains) }
            .flatten

          Set[Set[Boolean]](Set(), Set(true)) should contain(openedGroups.toSet)
        }
      }

      "Adjust each CornerLine" in {
        for (pg <- orthogonalPolygonFixtures.values) {
          val polygon: Polygon = normalizePolygon(pg)
          val corners: List[List[Corner]] = extractCorners(polygon)

          val (chords, lines): (List[Chord], List[CornerLine]) = {
            OrthogonalPolygonPartitioner.createChordsCornerLines(corners)
          }

          val horizontalLines: List[CornerLine] = lines.filterNot(_.pointsVertically)
          val verticalChordCorners: List[Corner] = {
            OrthogonalPolygonPartitioner.extractChordCorners(chords, vertical = true)
          }

          val groups: List[List[CornerGeometry]] = createLineSweepGroups {
            horizontalLines ::: verticalChordCorners
          }(vertical = false)

          val actions = groups.map(setActions(_, TreeSet())(false))
          val adjusted: Set[Point] = actions
            .flatMap { cn => cn.getOrElse("toAdjust", Nil).map(_.point) }
            .toSet

          val sourcePoints: Set[Point] = groups
            .flatMap(g => g collect { case cl: CornerLine => cl.source })
            .toSet

          sourcePoints shouldEqual adjusted
        }
      }
    }

    "adjustCornerGeometries" should {
      "make sure that adjusted CornerLines are not longer than the original -- horizontal" in {
        for (pg <- orthogonalPolygonFixtures.values) {
          val polygon: Polygon = normalizePolygon(pg)
          val corners: List[List[Corner]] = extractCorners(polygon)

          val (chords, lines): (List[Chord], List[CornerLine]) = {
            OrthogonalPolygonPartitioner.createChordsCornerLines(corners)
          }

          val horizontalLines: List[CornerLine] = lines.filterNot(_.pointsVertically)
          val verticalChordCorners: List[Corner] = {
            OrthogonalPolygonPartitioner.extractChordCorners(chords, vertical = true)
          }

          val adjustedCornerLines: List[CornerLine] = adjustCornerGeometries {
            horizontalLines ::: verticalChordCorners
          }(vertical = false)

          val matchedLines = adjustedCornerLines
            .sorted(CornerOrderingY)
            .zip(horizontalLines.sorted(CornerOrderingY))

          val isShorter = matchedLines.map { case (a, b) =>
            a.angle match {
              case 0 => a.dest.x <= b.dest.x
              case 180 => a.dest.x >= b.dest.x
            }
          }

          Set(Set(), Set(true)) should contain(isShorter.toSet)
        }
      }

      "make sure that adjusted CornerLines are not longer than the original -- vertical" in {
        for (pg <- orthogonalPolygonFixtures.values) {
          val polygon: Polygon = normalizePolygon(pg)
          val corners: List[List[Corner]] = extractCorners(polygon)

          val (chords, lines): (List[Chord], List[CornerLine]) = {
            OrthogonalPolygonPartitioner.createChordsCornerLines(corners)
          }

          val verticalLines: List[CornerLine] = lines.filter(_.pointsVertically)
          val horizontalChordCorners: List[Corner] = {
            OrthogonalPolygonPartitioner.extractChordCorners(chords, vertical = false)
          }

          val adjustedCornerLines: List[CornerLine] = adjustCornerGeometries {
            verticalLines ::: horizontalChordCorners
          }(vertical = true)

          val matchedLines = adjustedCornerLines
            .sorted(CornerOrderingX)
            .zip(verticalLines.sorted(CornerOrderingX))

          val isShorter = matchedLines.map { case (a, b) =>
            a.angle match {
              case 90 => a.dest.y <= b.dest.y
              case -90 => a.dest.y >= b.dest.y
            }
          }

          Set(Set(), Set(true)) should contain(isShorter.toSet)
        }
      }

      "have output CornerLines with the same source points as the input -- horizontal" in {
        for (pg <- orthogonalPolygonFixtures.values) {
          val polygon: Polygon = normalizePolygon(pg)
          val corners: List[List[Corner]] = extractCorners(polygon)

          val (chords, lines): (List[Chord], List[CornerLine]) = {
            OrthogonalPolygonPartitioner.createChordsCornerLines(corners)
          }

          val horizontalLines: List[CornerLine] = lines.filterNot(_.pointsVertically)
          val verticalChordCorners: List[Corner] = {
            OrthogonalPolygonPartitioner.extractChordCorners(chords, vertical = true)
          }

          val adjustedCornerLines: List[CornerLine] = adjustCornerGeometries {
            horizontalLines ::: verticalChordCorners
          }(vertical = false)

          val sourcesMatch = adjustedCornerLines
            .sorted(CornerOrderingY)
            .zip(horizontalLines.sorted(CornerOrderingY))
            .map { case (a, b) => a.source == b.source }

          Set(Set(), Set(true)) should contain(sourcesMatch.toSet)
        }
      }

      "have output CornerLines with the same source points as the input -- vertical" in {
        for (pg <- orthogonalPolygonFixtures.values) {
          val polygon: Polygon = normalizePolygon(pg)
          val corners: List[List[Corner]] = extractCorners(polygon)

          val (chords, lines): (List[Chord], List[CornerLine]) = {
            OrthogonalPolygonPartitioner.createChordsCornerLines(corners)
          }

          val verticalLines: List[CornerLine] = lines.filter(_.pointsVertically)
          val horizontalChordCorners: List[Corner] = {
            OrthogonalPolygonPartitioner.extractChordCorners(chords, vertical = false)
          }

          val adjustedCornerLines: List[CornerLine] = adjustCornerGeometries {
            verticalLines ::: horizontalChordCorners
          }(vertical = true)

          val sourcesMatch = adjustedCornerLines
            .sorted(CornerOrderingX)
            .zip(verticalLines.sorted(CornerOrderingX))
            .map { case (a, b) => a.source == b.source }

          Set(Set(), Set(true)) should contain(sourcesMatch.toSet)
        }
      }
    }
  }
}


class CornerExtenderSpec extends WordSpec with Matchers with PolygonFixtures {
  import OrthogonalPolygonCornerExtender.extendCorners
  import GeometryUtils.normalizePolygon


  def createLineString(cn: CornerLine): LineString = {
    GeometryUtils.geometryFactory.createLineString(
      Array(new Coordinate(cn.source.x, cn.source.y),
        new Coordinate(cn.dest.x, cn.dest.y))
    )
  }

  def createLineString(cn: Chord): LineString = {
    GeometryUtils.geometryFactory.createLineString(
      Array(new Coordinate(cn.source.x, cn.source.y),
            new Coordinate(cn.dest.x, cn.dest.y))
    )
  }

  "OrthogonalPolygonCornerExtender" can {
    "extendCorners" should {
      "extend the corners until they hit the boundary" in {
        val corners: List[Corner] = CornerExtractor
          .extractCorners(fixtures("approximatedPolygon")).head

        val startsVertically: Boolean = !corners.head.pointsVertically
        val vc: List[Corner] = if (startsVertically) corners.init else corners.tail

        val vEdges: Set[CornerLine] = extendCorners(vc)(extendVertically = false)
          .collect { case cl: CornerLine => cl }
          .toSet

        val expectedEdges: Set[CornerLine] = Set(
          CornerLine(Point(0.25, 1.5), Point(1.0, 1.5), 0),
          CornerLine(Point(0.5, 1.75), Point(1.0, 1.75), 0)
        )
        vEdges shouldEqual expectedEdges
      }

      "intersect the boundary at exactly two points" in {
        for (pg <- orthogonalPolygonFixtures.values) {
          val polygon: Polygon = normalizePolygon(pg)
          val corners: List[Corner] = CornerExtractor
            .extractCorners(polygon)
            .flatMap(_.tail)

          val vEdges = extendCorners(corners)(extendVertically = true)
          val hEdges = extendCorners(corners)(extendVertically = false)

          val lineStrings: List[LineString] = (vEdges ::: hEdges) collect {
            case cl: CornerLine => createLineString(cl)
            case ch: Chord => createLineString(ch)
          }

         val numIntersections: Set[Int] = lineStrings
            .map(polygon.getBoundary.intersection)
            .map(_.getNumPoints)
            .toSet

          Set(Set(), Set(2)) should contain (numIntersections)
        }
      }

      "create duplicate chords when the corners point in the same direction" in {
        for (polygonName <- List("complexPolygon0", "withChordsPolygon1")) {
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