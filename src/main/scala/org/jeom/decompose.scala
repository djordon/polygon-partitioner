package org.jeom

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

import com.vividsolutions.jts.geom.{Polygon, LineString}
import com.vividsolutions.jts.operation.polygonize.Polygonizer


object OrthogonalPolygonDecomposer {
  import GeometryUtils.IterablePolygon

  def extractChordCorners(ec: ExtendedCorner): List[Corner] = ec match {
    case ExtendedCorner(s, d, 0) => List(Corner(s, true, 0), Corner(d, true, 180))
    case ExtendedCorner(s, d, 180) => List(Corner(s, true, 180), Corner(d, true, 0))
    case ExtendedCorner(s, d, -90) => List(Corner(s, true, -90), Corner(d, true, 90))
    case ExtendedCorner(s, d, 90) => List(Corner(s, true, 90), Corner(d, true, -90))
  }

  def extractChords(corners: List[Corner], extendVertically: Boolean): List[ExtendedCorner] = {
    val concavePoints: Set[Point] = corners
      .filter(_.isConcave)
      .map(_.point)
      .toSet

    OrthononalPolygonCornerExtender
      .extendCorners(corners, extendVertically)
      .filter { ec => concavePoints.contains(ec.dest) }
  }

  def extractChords(pg: Polygon): List[ExtendedCorner] = {
    val corners = OrthogonalPolygonPartitioner.extractCorners(pg)
    val startsVertically: Boolean = corners.head.angle.abs != 90

    val hc: List[Corner] = if (startsVertically) corners.tail else corners.init
    val vc: List[Corner] = if (startsVertically) corners.init else corners.tail

    val vChords: List[ExtendedCorner] = extractChords(hc, true)
    val lChords: List[Corner] = vChords.flatMap(extractChordCorners)
    val hChords: List[ExtendedCorner] = extractChords(lChords ::: vc, false)

    vChords ::: hChords
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
    val edges: List[LineString] = chords ::: extractEdges(pg)

    val polygonizer = new Polygonizer()
    polygonizer.add(edges.asJavaCollection)
    
    polygonizer
      .getPolygons
      .toList
      .asInstanceOf[List[Polygon]]
      .map(PolygonApproximator.removeAxisAlignedColinearity(_))
  }
}
