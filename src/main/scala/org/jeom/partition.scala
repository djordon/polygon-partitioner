package org.jeom

import scala.collection.immutable.TreeSet
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.collection.Searching.{search, Found, InsertionPoint, SearchResult}

import com.vividsolutions.jts.geom.{Polygon, Coordinate, LineString}
import com.vividsolutions.jts.operation.polygonize.Polygonizer


object OrthononalPolygonCornerExtender {

  case class SweepContainer(
      openedCoords: TreeSet[Double],
      extendedCorners: List[ExtendedCorner],
      currentLevel: Double = Double.NegativeInfinity,
      closedCoords: Set[Double] = Set()) {

    def updateOpened(level: Double): TreeSet[Double] =
      if (level == currentLevel) openedCoords else openedCoords -- closedCoords

    def updateClosed(z: Double, contained: Boolean): Set[Double] =
      if (closedCoords.contains(z) && contained) closedCoords - z else closedCoords + z

    def updateCorners(cn: Corner, treeSet: TreeSet[Double]): List[ExtendedCorner] = {
      val destination: Point = cn.angle match {
        case 0 => Point(treeSet.from(cn.x).firstKey, cn.y)
        case 180 => Point(treeSet.to(cn.x).lastKey, cn.y)
        case -90 => Point(cn.x, treeSet.to(cn.y).lastKey)
        case 90 => Point(cn.x, treeSet.from(cn.y).firstKey)
      }

      ExtendedCorner(cn.point, destination, cn.angle) :: extendedCorners
    }

    def sweep(z: Double, level: Double, cn: Corner): SweepContainer = {
      val contained: Boolean = openedCoords contains z
      val closed: Set[Double] = updateClosed(z, contained)
      val opened: TreeSet[Double] = updateOpened(level)

      (level == currentLevel, contained) match {
        case (true, true) =>
          SweepContainer(opened, updateCorners(cn, opened - z), level, closed)
        case (true, false) =>
          SweepContainer(opened + z, updateCorners(cn, opened), level, closedCoords)
        case (false, true) =>
          SweepContainer(opened - z, updateCorners(cn, opened - z), level)
        case (false, false) =>
          SweepContainer(opened + z, updateCorners(cn, opened), level)
      }
    }

    def sweep(z: Double, level: Double): SweepContainer = {
      val contained: Boolean = openedCoords contains z
      val closed: Set[Double] = updateClosed(z, contained)

      (level == currentLevel, contained) match {
        case (true, true) => copy(closedCoords=closed)
        case (true, false) => copy(openedCoords=openedCoords + z)
        case (false, true) => SweepContainer(openedCoords -- closed, extendedCorners, level)
        case (false, false) => SweepContainer(openedCoords -- closed, extendedCorners, level)
      }
    }
  }

  private def sweepFolder(extendVertically: Boolean)(
      container: SweepContainer, 
      corner: Corner): SweepContainer = {

    val z: Double = if (extendVertically) corner.y else corner.x
    val l: Double = if (extendVertically) corner.x else corner.y
    val doExtend: Boolean = extendVertically == (corner.angle.abs == 90)

    (corner, doExtend) match {
      case (Corner(_, true, _), true) => container.sweep(z, l, corner)
      case _ => container.sweep(z, l)
    }
  }

  def extendCorners(corners: List[Corner], extendVertically: Boolean): List[ExtendedCorner] = {
    val ordering = if (extendVertically) CornerOrderingX else CornerOrderingY
    val emptyContainer: SweepContainer = SweepContainer(TreeSet(), Nil)

    corners
      .sorted(ordering)
      .foldLeft(emptyContainer)(sweepFolder(extendVertically))
      .extendedCorners
  }

  def extractChords(corners: List[Corner], extendVertically: Boolean): List[ExtendedCorner] = {
    val convexPoints: Set[Point] = corners
      .tail
      .filter(_.isConcave)
      .map(_.point)
      .toSet

    extendCorners(corners, extendVertically)
      .filter { ec => convexPoints.contains(ec.dest) }
  }
}


object OrthogonalPolygonDecomposer {
  import GeometryUtils.IterablePolygon

  def extractChordCorners(ec: ExtendedCorner): List[Corner] = ec match {
    case ExtendedCorner(s, d, 0) => List(Corner(s, true, 0), Corner(d, true, 180))
    case ExtendedCorner(s, d, 180) => List(Corner(s, true, 180), Corner(d, true, 0))
    case ExtendedCorner(s, d, -90) => List(Corner(s, true, -90), Corner(d, true, 90))
    case ExtendedCorner(s, d, 90) => List(Corner(s, true, 90), Corner(d, true, -90))
  }

  def extractChords(corners: List[Corner], extendVertically: Boolean): List[ExtendedCorner] = {
    val convexPoints: Set[Point] = corners
      .filter(_.isConcave)
      .map(_.point)
      .toSet

    OrthononalPolygonCornerExtender
      .extendCorners(corners, extendVertically)
      .filter { ec => convexPoints.contains(ec.dest) }
  }

  def extractChords(pg: Polygon): List[ExtendedCorner] = {
    val corners = OrthogonalPolygonPartitioner.extractCorners(pg)
    val startsVertically: Boolean = corners.head.angle.abs != 90

    val hc: List[Corner] = if (startsVertically) corners.tail else corners.init
    val vc: List[Corner] = if (startsVertically) corners.init else corners.tail

    val vChords: List[ExtendedCorner] = extractChords(hc, true)
    val lChords: List[Corner] = vChords.flatMap(extractChordCorners)
    val hChords: List[ExtendedCorner] = extractChords(lChords ++ vc, false)

    vChords ++ hChords
  }

  def extractEdges(pg: Polygon): List[LineString] = {
    pg.sliding(2, 1).map { points =>
      GeometryUtils
        .geometryFactory
        .createLineString(Array(points.head, points.last))
        .asInstanceOf[LineString]
    }.toList
  }

  def decompose(pg: Polygon): List[Polygon] = {
    val chords: List[LineString] = extractChords(pg).map(_.toLineString)
    val edges: List[LineString] = chords ++ extractEdges(pg)

    val polygonizer = new Polygonizer()
    polygonizer.add(edges.asJavaCollection)
    
    polygonizer
      .getPolygons
      .toList
      .asInstanceOf[List[Polygon]]
      .map(PolygonApproximator.removeAxisAlignedColinearity(_))
  }
}


object OrthogonalPolygonPartitioner {
  import scala.language.implicitConversions
  import GeometryUtils.IterablePolygon

  implicit def pointToListPoint(pt: Point): List[Point] = List(pt)

  case class EndpointStacks(
      upperLeft: List[Point] = Nil,
      lowerLeft: List[Point] = Nil,
      lowerRight: List[Point] = Nil) {

    def prepend(ul: List[Point] = Nil, ll: List[Point] = Nil, lr: List[Point] = Nil) =
      EndpointStacks(ul ::: upperLeft, ll ::: lowerLeft, lr ::: lowerRight)
  }

  def extractCorners(polygon: Polygon): List[Corner] = {
    val boundary: List[Coordinate] = polygon.toList.tail
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

  private def makeRectangleCorners(corners: List[Corner]): List[CornerPoint] = {
    val startsVertically: Boolean = corners.head.angle.abs != 90

    val hc: List[Corner] = if (startsVertically) corners.tail else corners.init
    val vc: List[Corner] = if (startsVertically) corners.init else corners.tail

    val vEdges: List[ExtendedCorner] = OrthononalPolygonCornerExtender
      .extendCorners(hc, extendVertically=true)

    val hEdges: List[ExtendedCorner] = OrthononalPolygonCornerExtender
      .extendCorners(vEdges.flatMap(_.toListCorner) ++ vc, extendVertically=false)

    vEdges ++ hEdges ++ corners.tail.filterNot(_.isConcave)
  }

  private def cornerFolder2(
      stacks: EndpointStacks,
      corner: CornerPoint): EndpointStacks = {

    corner match {
      case ExtendedCorner(source, dest, 0) => stacks.prepend(ll=source, lr=dest)
      case ExtendedCorner(source, dest, 90) => stacks.prepend(ul=dest, lr=source)
      case ExtendedCorner(source, dest, 180) => stacks.prepend(ul=dest, ll=dest)
      case ExtendedCorner(source, dest, -90) => stacks.prepend(ul=source, ll=dest, lr=dest)
      case Corner(source, false, 90) => stacks.prepend(ul=source)
      case Corner(source, false, 180) => stacks.prepend(ll=source)
      case Corner(source, false, -90) => stacks.prepend(lr=source)
      case _ => stacks
    }
  }

  private def cornerFolder(
      stacks: Tuple3[List[Point], List[Point], List[Point]],
      corner: CornerPoint): Tuple3[List[Point], List[Point], List[Point]] = {

    corner match {
      case ExtendedCorner(source, dest, 0) =>
        (stacks._1, source :: stacks._2, dest :: stacks._3)
      case ExtendedCorner(source, dest, 90) =>
        (dest :: stacks._1, stacks._2, source :: stacks._3)
      case ExtendedCorner(source, dest, 180) =>
        (dest :: stacks._1, dest :: stacks._2, stacks._3)
      case ExtendedCorner(source, dest, -90) =>
        (source :: stacks._1, dest :: stacks._2, dest :: stacks._3)
      case Corner(source, false, 90) =>
        (source :: stacks._1, stacks._2, stacks._3)
      case Corner(source, false, 180) =>
        (stacks._1, source :: stacks._2, stacks._3)
      case Corner(source, false, -90) =>
        (stacks._1, stacks._2, source :: stacks._3)
      case _ => stacks
    }
  }

  private def extractIndex(sr: SearchResult): Int = sr match { 
    case Found(i) => i + 1
    case InsertionPoint(i) => i
  }

  private def extractRectangles(cornersPoints: List[CornerPoint]): List[Rectangle] = {
    // val es: EndpointStacks = EndpointStacks()
    val es: Tuple3[List[Point], List[Point], List[Point]] = (Nil, Nil, Nil)
  
    val (upperLeft, lowerLeft, lowerRight) = cornersPoints.foldLeft(es)(cornerFolder) //match {
        // case EndpointStacks(ul, ll, lr) => (ul, ll, lr)
    // }
    val ul: Vector[Point] = upperLeft.toVector.sorted(PointOrderingX)
    val lr: Vector[Point] = lowerRight.toVector.sorted(PointOrderingY)
  
    lowerLeft map { p =>
      val i1 = extractIndex(ul.search(p)(PointOrderingX))
      val i2 = extractIndex(lr.search(p)(PointOrderingY))
      Rectangle(ul(i1), lr(i2))
    }
  }

  def partitionSimple: Polygon => List[Rectangle] =
    extractCorners _ andThen makeRectangleCorners _ andThen extractRectangles _

  def partition(pg: Polygon): List[Rectangle] = OrthogonalPolygonDecomposer
    .decompose(pg)
    .flatMap(partitionSimple)
}
