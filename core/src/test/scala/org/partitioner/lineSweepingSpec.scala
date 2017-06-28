package org.partitioner

import org.scalatest.{Matchers, WordSpec}
import com.vividsolutions.jts.geom.Polygon
import org.partitioner.partition._

import scala.collection.immutable.TreeSet
import scala.language.reflectiveCalls


class LineSweepingSpec extends WordSpec with Matchers with PolygonFixtures {

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

          Set[Set[Boolean]](Set(), Set(true)) should contain (openedGroups.toSet)
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
            .flatMap(g => g collect { case cl: CornerLine => cl.source } )
            .toSet

          sourcePoints shouldEqual adjusted
        }
      }
    }

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

          val groups: List[List[CornerGeometry]] = createLineSweepGroups
            { horizontalLines ::: verticalChordCorners } (vertical = false)

          val isIncreasing: Iterator[Boolean] = groups
            .flatMap(g => g.map(_.y))
            .sliding(2, 1)
            .map(g => g.head <= g.last)

          Set(Set(), Set(true)) should contain (isIncreasing.toSet)
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

          Set(Set(), Set(true)) should contain (isShorter.toSet)
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

          Set(Set(), Set(true)) should contain (sourcesMatch.toSet)
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

          Set(Set(), Set(true)) should contain (sourcesMatch.toSet)
        }
      }
    }
  }
}