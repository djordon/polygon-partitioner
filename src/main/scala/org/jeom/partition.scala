package org.jeom

import scala.collection.immutable.TreeSet
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.collection.Searching.{search, Found, InsertionPoint, SearchResult}

import com.vividsolutions.jts.geom.{Polygon, Coordinate, LineString}
import com.vividsolutions.jts.operation.polygonize.Polygonizer


object OrthononalPolygonCornerExtender {
  case class SweepContainer(
      opened: TreeSet[Double],
      extendedCorners: List[ExtendedCorner],
      currentLevel: Double = Double.NegativeInfinity,
      closed: Set[Double] = Set()) {

    def updateOpened(level: Double): TreeSet[Double] =
      if (level == currentLevel) opened else opened -- closed

    def extend(corner: Corner, updatedOpened: TreeSet[Double]): List[ExtendedCorner] =
      extendCorner(corner, updatedOpened) :: extendedCorners

    def sweep(z: Double, level: Double, cn: Corner): SweepContainer = {
      val updatedOpened: TreeSet[Double] = updateOpened(level)
      val zOpened: Boolean = opened contains z

      val newClosed: Set[Double] = if (closed.contains(z) && zOpened) closed - z else closed + z

      (level == currentLevel, zOpened) match {
        case (true, true) =>
          SweepContainer(updatedOpened, extend(cn, updatedOpened - z), level, newClosed)
        case (true, false) =>
          copy(opened=updatedOpened + z, extendedCorners=extend(cn, updatedOpened))
        case (false, true) =>
          SweepContainer(updatedOpened - z, extend(cn, updatedOpened - z), level)
        case (false, false) =>
          SweepContainer(updatedOpened + z, extend(cn, updatedOpened), level)
      }
    }

    def sweep(z: Double, level: Double): SweepContainer = {
      val zOpened: Boolean = opened contains z
      val newClosed: Set[Double] = if (closed.contains(z) && zOpened) closed - z else closed + z

      (level == currentLevel, opened contains z) match {
        case (true, true) => copy(closed=newClosed)
        case (true, false) => copy(opened=opened + z)
        case (false, true) => SweepContainer(opened -- newClosed, extendedCorners, level)
        case (false, false) => SweepContainer(opened -- closed + z, extendedCorners, level)
      }
    }
  }

  private def extendCorner(cn: Corner, treeSet: TreeSet[Double]): ExtendedCorner = {
    val destination: Point = cn.angle match {
      case 0 => Point(treeSet.from(cn.x).firstKey, cn.y)
      case 180 => Point(treeSet.to(cn.x).lastKey, cn.y)
      case -90 => Point(cn.x, treeSet.to(cn.y).lastKey)
      case 90 => Point(cn.x, treeSet.from(cn.y).firstKey)
    }

    ExtendedCorner(cn.point, destination, cn.angle)
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
    val convexPoints: Set[Point] = corners.tail.filter(_.isConvex).map(_.point).toSet

    extendCorners(corners, extendVertically) filter { ec => convexPoints.contains(ec.dest) }
  }
}


object OrthogonalPolygonDecomposer {
  import GeometryUtils.IterablePolygon

  def extractChordCorner(ec: ExtendedCorner): List[Corner] = ec match {
    case ExtendedCorner(s, d, 0) => List(Corner(s, true, 0), Corner(d, true, 180))
    case ExtendedCorner(s, d, 180) => List(Corner(s, true, 180), Corner(d, true, 0))
    case ExtendedCorner(s, d, -90) => List(Corner(s, true, -90), Corner(d, true, 90))
    case ExtendedCorner(s, d, 90) => List(Corner(s, true, 90), Corner(d, true, -90))
  }

  def extractChords(corners: List[Corner], extendVertically: Boolean): List[ExtendedCorner] = {
    val convexPoints: Set[Point] = corners
      .filter(_.isConvex)
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
    val lChords: List[Corner] = vChords.flatMap(extractChordCorner)
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
  import GeometryUtils.IterablePolygon

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

    vEdges ++ hEdges ++ corners.tail.filterNot(_.isConvex)
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
    val init: Tuple3[List[Point], List[Point], List[Point]] = (Nil, Nil, Nil)
  
    val (upperLeft, lowerLeft, lowerRight) = cornersPoints.foldLeft(init)(cornerFolder)
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
