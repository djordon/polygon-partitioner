package org.partitioner

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

import com.vividsolutions.jts.geom.{Polygon, LineString}
import com.vividsolutions.jts.operation.polygonize.Polygonizer


object OrthogonalPolygonDecomposer {
  import GeometryUtils.IterablePolygon
  import OrthogonalPolygonPartitioner.orderCorners
  import OrthogonalPolygonCornerExtender.extendCorners

  private def uniqueExtendedCorners(
      set: Set[ExtendedCorner], ec: ExtendedCorner): Set[ExtendedCorner] = {

    if (set contains ec.swap) set else set + ec
  }

  def extractChords(corners: List[Corner], extendVertically: Boolean): List[ExtendedCorner] = {
    val concavePoints: Set[Point] = corners
      .filter(_.isConcave)
      .map(_.point)
      .toSet

    val init: Set[ExtendedCorner] = Set[ExtendedCorner]()

//    OrthogonalPolygonCornerExtender
//      .extendCorners(corners)(extendVertically)
//      .filter { ec => concavePoints.contains(ec.dest) }
//      .foldLeft(init)(uniqueExtendedCorners)
    init.toList
  }

  def extractChords(pg: Polygon): List[Chord] = {
    val corners = OrthogonalPolygonPartitioner.extractCorners(pg)
    val hc: List[Corner] = corners.flatMap(orderCorners(_, vertically = false))
    val vc: List[Corner] = corners.flatMap(orderCorners(_, vertically = true))

    val vChords: List[Chord] = extendCorners(hc)(true)
      .filter(_.isInstanceOf[Chord]).asInstanceOf[List[Chord]]

    val lChords: List[Corner] = vChords.flatMap(_.toListCorner)

    val hChords: List[Chord] = extendCorners(lChords ::: vc)(false)
      .filter(_.isInstanceOf[Chord]).asInstanceOf[List[Chord]]

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
