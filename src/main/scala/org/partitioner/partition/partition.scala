package org.partitioner.partition

import com.vividsolutions.jts.geom.{Coordinate, Polygon}
import org.partitioner._

import scala.collection.Searching.{Found, InsertionPoint, SearchResult, search}


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
    val holes: List[List[Coordinate]] = polygon.getHoles.map(_.toList.tail.reverse)

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
  import RectangleEndpointExtractor.extractRectangleEndpoints
  import scala.language.implicitConversions

  def orderCorners(corners: List[Corner], vertically: Boolean): List[Corner] = {
    val startsVertically: Boolean = corners.head.angle.abs != 90
    if (startsVertically == vertically) corners.init else corners.tail
  }

  def extractInternalEdges(corners: List[List[Corner]], verticals: Boolean)
      : List[CornerGeometry] = {

    val co: List[Corner] = corners.flatMap(orderCorners(_, vertically = verticals))

    OrthogonalPolygonCornerExtender
      .extendCorners(co)(extendVertically = !verticals)
  }

  def makeRectangleCorners(corners: List[List[Corner]]): List[CornerGeometry] = {

    val vEdges: List[CornerGeometry] = extractInternalEdges(corners, true)
    val hEdges: List[CornerGeometry] = extractInternalEdges(corners, false)

    val chords: List[Chord] = OrthogonalPolygonChordReducer.reduceChords {
      (vEdges ::: hEdges) collect { case c: Chord => c }
    }

    val chordCorners: List[Corner] = chords
      .filter(_.angle.abs != 90)
      .flatMap(_.toListCorner)
      .map { _.copy(isConcave = false) }

    val chordPoints: Set[Point] = chords
      .flatMap(_.toListCorner)
      .map(_.point)
      .toSet

    val lines: List[CornerLine] = (vEdges ::: hEdges) collect {
      case ec: CornerLine if !chordPoints.contains(ec.source) => ec
    }

    val convexCorners = corners.flatMap(_.tail).filterNot(_.isConcave)
    val horizontalLines = lines.filter(_.angle.abs != 90)
    val verticalLines = CornerLineAdjuster.adjustCornersLines
      { horizontalLines.flatMap(_.toListCorner) ::: lines.filter(_.angle.abs == 90) ::: chordCorners }

    chords ::: verticalLines ::: horizontalLines ::: convexCorners
  }

  private def extractIndex(sr: SearchResult): Int = sr match {
    case Found(i) => i + 1
    case InsertionPoint(i) => i
  }

  private def extractRectangles(cornersPoints: List[CornerGeometry]): List[Rectangle] = {
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

  def partition: Polygon => List[Rectangle] =
    extractCorners _ andThen makeRectangleCorners _ andThen extractRectangles _

  def partitionChorded(pg: Polygon): List[Rectangle] = OrthogonalPolygonDecomposer
    .decompose(pg)
    .flatMap(partition)
}
