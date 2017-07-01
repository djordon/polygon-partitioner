package org.partitioner.partition

import com.vividsolutions.jts.geom.{Coordinate, Polygon}
import org.partitioner._

import scala.collection.Searching.{Found, InsertionPoint, SearchResult, search}
import scala.language.implicitConversions

case class EndpointStacks(
    upperLeft: List[Point] = Nil,
    lowerLeft: List[Point] = Nil,
    lowerRight: List[Point] = Nil) {

  def prepend(ul: List[Point] = Nil, ll: List[Point] = Nil, lr: List[Point] = Nil) =
    EndpointStacks(ul ::: upperLeft, ll ::: lowerLeft, lr ::: lowerRight)
}


object CornerExtractor {
  import GeometryUtils.IterablePolygon

  def makeCorners(boundary: List[Coordinate]): List[Corner] = {
    val extended: List[Coordinate] = Stream
      .continually(boundary)
      .flatten
      .take(boundary.length + 3)
      .toList

    extended
      .sliding(3, 1)
      .map(Corner.apply)
      .toList
  }

  def extractCorners(polygon: Polygon): List[List[Corner]] = {
    val boundary: List[Coordinate] = polygon.toList.tail
    val holes: List[List[Coordinate]] = polygon.getHolesCoordinates.map(_.tail)

    (boundary :: holes).map(makeCorners)
  }
}


object RectangleEndpointExtractor {
  implicit def pointToListPoint(pt: Point): List[Point] = List(pt)

  private def stackExtendedCorner(ex: CornerLine, stacks: EndpointStacks): EndpointStacks = {
    ex match {
      case CornerLine(s, d, 0) => stacks.prepend(ll=s, lr=d)
      case CornerLine(s, d, 90) => stacks.prepend(ul=d, lr=s)
      case CornerLine(s, d, 180) => stacks.prepend(ul=d, ll=d)
      case CornerLine(s, d, -90) => stacks.prepend(ul=s, ll=d, lr=d)
      case _ => stacks
    }
  }

  private def stackCorner(cn: Corner, stacks: EndpointStacks): EndpointStacks = {
    cn match {
      case Corner(s, false, 90) => stacks.prepend(ul=s)
      case Corner(s, false, 180) => stacks.prepend(ll=s)
      case Corner(s, false, -90) => stacks.prepend(lr=s)
      case _ => stacks
    }
  }

  private def stackChord(ch: Chord, stacks: EndpointStacks): EndpointStacks = {
    ch match {
      case Chord(Corner(s, _, 90), Corner(d, _, 0)) => stacks.prepend(lr=s)
      case Chord(Corner(s, _, 90), Corner(d, _, -90)) => stacks.prepend(lr=s, ul=d)
      case Chord(Corner(s, _, -90), Corner(d, _, 90)) => stacks.prepend(lr=d, ul=s)
      case Chord(Corner(s, _, -90), Corner(d, _, 180)) => stacks.prepend(ll=d, ul=s)
      case Chord(Corner(s, _, 180), Corner(d, _, 90)) => stacks.prepend(ul=d)
      case Chord(Corner(s, _, 180), Corner(d, _, 0)) => stacks.prepend(ll=d)
      case Chord(Corner(s, _, 0), Corner(d, _, 180)) => stacks.prepend(ll=s)
      case Chord(Corner(s, _, 0), Corner(d, _, -90)) => stacks.prepend(ll=s, lr=d)
      case _ => stacks
    }
  }

  private def cornerFolder(
      stacks: EndpointStacks,
      corner: CornerGeometry): EndpointStacks = {

    corner match {
      case ex: CornerLine => stackExtendedCorner(ex, stacks)
      case cn: Corner => stackCorner(cn, stacks)
      case ch: Chord => stackChord(ch, stacks)
    }
  }

  def extractRectangleEndpoints(cornersPoints: List[CornerGeometry]): EndpointStacks = {
    cornersPoints.foldLeft(EndpointStacks())(cornerFolder)
  }
}

object OrthogonalPolygonPartitioner {
  import CornerExtractor.extractCorners
  import GeometryUtils.normalizePolygon
  import RectangleEndpointExtractor.extractRectangleEndpoints
  import OrthogonalPolygonCornerExtender.extendCorners
  import scala.language.implicitConversions

  def createInteriorLines(corners: List[List[Corner]]): List[CornerGeometry] = {
    val co: List[Corner] = corners.flatMap(_.tail)

    extendCorners(co)(true) ::: extendCorners(co)(false)
  }

  def extractChordCorners(chords: List[Chord], vertical: Boolean): List[Corner] = {
    chords
      .filter { _.pointsVertically == vertical }
      .flatMap { _.toListCorner }
      .map { _.copy(isConcave = false) }
  }

  def separateChordsCornerLines(interiorLines: List[CornerGeometry])
      : (List[Chord], List[CornerLine]) = {

    val chords: List[Chord] = OrthogonalPolygonChordReducer.reduceChords {
      interiorLines collect { case c: Chord => c }
    }

    val chordPoints: Set[Point] = chords
      .flatMap(_.toListCorner)
      .map(_.point)
      .toSet

    val lines: List[CornerLine] = interiorLines collect {
      case ec: CornerLine if !chordPoints.contains(ec.source) => ec
      case ch: Chord if !(chordPoints contains ch.point) => ch.toCornerLine
    }

    (chords, lines)
  }

  def createChordsCornerLines: List[List[Corner]] => (List[Chord], List[CornerLine]) = {
    (createInteriorLines _) andThen (separateChordsCornerLines _)
  }

  def makeRectangleCorners(corners: List[List[Corner]]): List[CornerGeometry] = {

    val (chords, lines) = createChordsCornerLines(corners)

    val horizontalLines: List[CornerLine] = CornerLineAdjuster.adjustCornerGeometries
      { lines.filterNot(_.pointsVertically) ::: extractChordCorners(chords, true) }
      { false }

    val verticalLines: List[CornerLine] = CornerLineAdjuster.adjustCornerGeometries
      { horizontalLines.flatMap(_.toListCorner) :::
          lines.filter(_.pointsVertically) :::
          extractChordCorners(chords, false) }
      { true }

    val convexCorners = corners.flatMap(_.tail).filterNot(_.isConcave)

    chords ::: verticalLines ::: horizontalLines ::: convexCorners
  }

  private def extractIndex(sr: SearchResult): Int = sr match {
    case Found(i) => i + 1
    case InsertionPoint(i) => i
  }

  def extractRectangles(cornersPoints: List[CornerGeometry]): List[Rectangle] = {
    val (upperLeft, lowerLeft, lowerRight) = extractRectangleEndpoints(cornersPoints) match {
        case EndpointStacks(ul, ll, lr) => (ul, ll, lr)
    }
    val ul: Vector[Point] = upperLeft.toVector.sorted(PointOrderingX)
    val lr: Vector[Point] = lowerRight.toVector.sorted(PointOrderingY)

    lowerLeft map { p =>
      val i1 = extractIndex(ul.search(p)(PointOrderingX))
      val i2 = extractIndex(lr.search(p)(PointOrderingY))
      Rectangle(ul(i1), lr(i2))
    }
  }

  def partition: Polygon => List[Rectangle] = (normalizePolygon _)
    .andThen(extractCorners _)
    .andThen(makeRectangleCorners _)
    .andThen(extractRectangles _)
}


object PolygonPartitioner {
  import CornerExtractor.extractCorners
  import OrthogonalPolygonPartitioner.{extractRectangles, makeRectangleCorners}
  import OrthogonalPolygonBuilder.approximate

  def partition(
      polygon: Polygon,
      simplifyTolerance: Double = 0,
      densifyTolerance: Double = Double.PositiveInfinity,
      size: Int = 3,
      step: Int = 1): List[Rectangle] = {

    (extractCorners _)
      .andThen(makeRectangleCorners _)
      .andThen(extractRectangles _)(
        approximate(polygon, simplifyTolerance, densifyTolerance, size, step)
      )
  }
}