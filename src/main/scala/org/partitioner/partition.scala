package org.partitioner

import scala.annotation.switch
import scala.collection.immutable.TreeSet
import scala.collection.Searching.{search, Found, InsertionPoint, SearchResult}

import com.vividsolutions.jts.geom.{Polygon, Coordinate}


object OrthogonalPolygonCornerExtender {

  private val emptyLines: List[ExtendedCorner] = Nil

  case class LineContainer(
      openedCoords: TreeSet[Double] = TreeSet[Double](),
      extendedCorners: List[ExtendedCorner] = Nil)

  def extendCorner(treeSet: TreeSet[Double])(cn: Corner): ExtendedCorner = {

    val destination: Point = (cn.angle: @switch) match {
      case 0 => Point((treeSet - cn.x).from(cn.x).firstKey, cn.y)
      case 180 => Point((treeSet - cn.x).to(cn.x).lastKey, cn.y)
      case -90 => Point(cn.x, (treeSet - cn.y).to(cn.y).lastKey)
      case 90 => Point(cn.x, (treeSet - cn.y).from(cn.y).firstKey)
    }

    ExtendedCorner(cn.point, destination, cn.angle)
  }

  def setActions(corners: List[Corner], opened: TreeSet[Double])(
      implicit extendVertically: Boolean): Map[String, List[Corner]] = {

    val toOpenClose: Map[String, List[Corner]] = corners
      .groupBy(cn => cn.point)
      .filter(_._2.length == 1)
      .map(_._2.head)
      .toList
      .groupBy(cn => if (opened.contains(cn.z)) "toClose" else "toOpen")

    val toExtend: List[Corner] = corners filter { cn =>
      (extendVertically == (cn.angle.abs == 90)) && cn.isConcave
    }

    toOpenClose + ("toExtend" -> toExtend)
  }

  private def lineSweeper(container: LineContainer, corners: List[Corner])(
      implicit extendVertically: Boolean): LineContainer = {

    val actions = setActions(corners, container.openedCoords)

    val opened: TreeSet[Double] = container.openedCoords ++
      actions.getOrElse("toOpen", Nil).map(_.z)

    val closed: TreeSet[Double] = opened --
      actions.getOrElse("toClose", Nil).map(_.z)

    val extended: List[ExtendedCorner] = actions
      .getOrElse("toExtend", Nil)
      .map(extendCorner(opened))

    LineContainer(closed, extended ::: container.extendedCorners)
  }

  def extendCorners(corners: List[Corner])(
      implicit extendVertically: Boolean): List[ExtendedCorner] = {

    corners
      .groupBy(_.z(!extendVertically))
      .toList
      .sortBy(_._1)
      .map(_._2)
      .foldLeft(LineContainer())(lineSweeper)
      .extendedCorners
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

  def orderCorners(corners: List[Corner], vertically: Boolean): List[Corner] = {
    val startsVertically: Boolean = corners.head.angle.abs != 90
    if (startsVertically == vertically) corners.init else corners.tail
  }

  def makeRectangleCorners(corners: List[List[Corner]]): List[CornerPoint] = {
    val hc: List[Corner] = corners.flatMap(orderCorners(_, vertically = false))
    val vc: List[Corner] = corners.flatMap(orderCorners(_, vertically = true))

    val vEdges: List[ExtendedCorner] = OrthogonalPolygonCornerExtender
      .extendCorners(hc)(extendVertically=true)

    val hEdges: List[ExtendedCorner] = OrthogonalPolygonCornerExtender
      .extendCorners(vEdges.flatMap(_.toListCorner) ::: vc)(extendVertically=false)

    vEdges ::: hEdges ::: corners.flatten.tail.filterNot(_.isConcave)
  }

  private def cornerFolder(
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

  private def extractIndex(sr: SearchResult): Int = sr match {
    case Found(i) => i + 1
    case InsertionPoint(i) => i
  }

  private def extractRectangles(cornersPoints: List[CornerPoint]): List[Rectangle] = {
    val es: EndpointStacks = EndpointStacks()

    val (upperLeft, lowerLeft, lowerRight) = cornersPoints.foldLeft(es)(cornerFolder) match {
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

  def partitionSimple: Polygon => List[Rectangle] =
    extractCorners _ andThen makeRectangleCorners _ andThen extractRectangles _

  def partition(pg: Polygon): List[Rectangle] = OrthogonalPolygonDecomposer
    .decompose(pg)
    .flatMap(partitionSimple)
}
