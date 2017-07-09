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


trait RectangleEndpointExtractor {
  implicit def pointToListPoint(pt: Point): List[Point] = List(pt)

  private[this] def stackExtendedCorner(ex: CornerLine, stacks: EndpointStacks): EndpointStacks = {
    ex match {
      case CornerLine(s, d, 0) => stacks.prepend(ll=s, lr=d)
      case CornerLine(s, d, 90) => stacks.prepend(ul=d, lr=s)
      case CornerLine(s, d, 180) => stacks.prepend(ul=d, ll=d)
      case CornerLine(s, d, -90) => stacks.prepend(ul=s, ll=d, lr=d)
      case _ => stacks
    }
  }

  private[this] def stackCorner(cn: Corner, stacks: EndpointStacks): EndpointStacks = {
    cn match {
      case Corner(s, false, 90) => stacks.prepend(ul=s)
      case Corner(s, false, 180) => stacks.prepend(ll=s)
      case Corner(s, false, -90) => stacks.prepend(lr=s)
      case _ => stacks
    }
  }

  private[this] def stackChord(ch: Chord, stacks: EndpointStacks): EndpointStacks = {
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

  private[this] def cornerFolder(
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

object OrthogonalPolygonPartitioner extends RectangleEndpointExtractor {
  import CornerExtractor.extractCorners
  import PolygonApproximator.removeAxisAlignedCollinearity
  import OrthogonalPolygonCornerExtender.extendCorners

  private[this] lazy val axisAlignedAngles: Set[Double] = Set(0.0, 90.0, 180.0, -90.0)

  def isOrthogonalPolygon(polygon: Polygon): Boolean = {
    val vertices: List[Vertex] = PolygonApproximator.polygon2Vertices(polygon)
    vertices.map(_.angle).toSet == axisAlignedAngles
  }

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

  private[this] def extractIndex(sr: SearchResult): Int = sr match {
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

  private[partitioner] def partitionLiteral: Polygon => List[Rectangle] = (extractCorners _)
      .andThen(makeRectangleCorners _)
      .andThen(extractRectangles _)

  /**
   * Partitions an orthogonal polygon into a list of non-overlapping
   * rectangles.
   *
   * @param polygon the orthogonal polygon to partition
   * @return Returns a list of non-overlapping rectangles
   */
  def partition(polygon: Polygon): List[Rectangle] =
    partitionLiteral { removeAxisAlignedCollinearity(polygon) }
}


object PolygonPartitioner {
  import OrthogonalPolygonPartitioner.partitionLiteral
  import OrthogonalPolygonBuilder.cover

  /**
   * Returns a list of non-overlapping rectangles that cover the input
   * polygon.
   *
   * Holes in the input polygon need not be in output polygon. This method
   * can be really slow when holes are present.
   *
   * @param polygon The input polygon
   * @param size a value that sets how coarse the output should be.
   *             The larger the value, the more coarse the out put will be.
   *             Must be greater than 2.
   * @param step a value that helps set how coarse the output should be.
   *             The larger the value, the more coarse the out put will be.
   *             Must be greater than 0 and less size - 1.
   * @return Returns a list of non-overlapping rectangles
   */
  def partition(polygon: Polygon, size: Int = 3, step: Int = 1): List[Rectangle] =
    partitionLiteral { cover(polygon, size, step) }
}
