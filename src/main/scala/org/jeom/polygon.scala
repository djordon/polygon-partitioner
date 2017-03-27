package org.jeom

import scala.collection.JavaConverters._

import com.vividsolutions.jts.simplify.DouglasPeuckerSimplifier
import com.vividsolutions.jts.triangulate.DelaunayTriangulationBuilder
import com.vividsolutions.jts.geom.{GeometryFactory, GeometryCollection, Geometry, Polygon, Coordinate, LineString, LinearRing}
import com.vividsolutions.jts.operation.union.CascadedPolygonUnion


class OrthogonalPolygon(polygon: Polygon) {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")
  }
}


case class Vec(coordinate: Coordinate, isVertical: Boolean)


object Vec {
  def vecConstructor(a: List[Coordinate]): Vec = Vec(a.last, a.head.x == a.last.x)

  def vecFilter(a: List[Vec], b: Vec): List[Vec] =
    if (a.head.isVertical == b.isVertical) b :: a.tail else b :: a
}


object OrthogonalPolygonBuilder2 {
  import GeometryUtils.{IterableCollection, IterablePolygon}

  val tol: Double = scala.math.pow(2, -13)
  val geometryFactory = new GeometryFactory()

  def build(geometry: Geometry, distanceTolerance: Double = tol): Polygon = {
    val simplerGeometry = DouglasPeuckerSimplifier.simplify(geometry, distanceTolerance)
    val firstCover = cover(simplerGeometry)

    cover(DouglasPeuckerSimplifier.simplify(firstCover, 2 * tol))
  }

  def cover(geometry: Geometry): Polygon = {
    val triangulator = new DelaunayTriangulationBuilder()
    triangulator.setSites(geometry)

    val rectangularCover: Iterable[Geometry] = triangulator
      .getTriangles(geometryFactory)
      .asInstanceOf[GeometryCollection]
      .map(_.getEnvelope)

    CascadedPolygonUnion
      .union(rectangularCover.asJavaCollection)
      .asInstanceOf[Polygon]
  }
}



object OrthogonalPolygonBuilder3 {
  import GeometryUtils.IterablePolygon

  val tol: Double = scala.math.pow(2, -13)
  val geometryFactory = new GeometryFactory()

  def getMaxOrdinate(polygon: Polygon): Double = {
    1.0 + polygon
      .getEnvelope.asInstanceOf[Polygon]
      .toList
      .flatMap(c => List(c.x, c.y))
      .map(math.abs)
      .max
  }

  def potentialTangents(c: Coordinate, sup: Double): List[LineString] = {
    val poles = List(new Coordinate(c.x, sup), new Coordinate(c.x, -sup),
                    new Coordinate(sup, c.y), new Coordinate(-sup, c.y))
    poles map { p => geometryFactory.createLineString(Array(p, c)) }
  }

  def filterTangents(lines: List[LineString], boundary: Geometry): List[LineString] =
    lines.filter(_.touches(boundary))

  def lines2rectangle(lines: List[LineString]): Geometry = {
    geometryFactory
      .createLineString(lines.flatMap(_.getCoordinates).toArray)
      .getEnvelope
  }

  def makeTangentRectangles(polygon: Polygon, tolerance: Double) = {
    val simpler: Polygon = DouglasPeuckerSimplifier
      .simplify(polygon, tolerance)
      .asInstanceOf[Polygon]

    val sup: Double = getMaxOrdinate(polygon)

    simpler
      .map(potentialTangents(_, sup))
      .map(filterTangents(_, polygon))
      .filter(_.length > 1)
      .map(lines2rectangle)
  }

  def build(polygon: Polygon, tolerance: Double = tol): Polygon = {
    val rectangles: Iterable[Geometry] = makeTangentRectangles(polygon, tolerance)
    val boundary: Array[Coordinate] = CascadedPolygonUnion
      .union(rectangles.asJavaCollection)
      .asInstanceOf[Polygon]
      .getInteriorRingN(0)
      .getCoordinates

    geometryFactory.createPolygon(boundary)
  }
}


object OrthogonalPolygonSimplifier {
  import GeometryUtils.IterablePolygon
  val geometryFactory = new GeometryFactory()

  private def polygon2vecs(pg: Polygon): List[Vec] = {
    pg.toList
      .sliding(2, 1)
      .map(Vec.vecConstructor)
      .toList
  }

  private def filterVecs(vecs: List[Vec]): List[Vec] = {
    val reduced: List[Vec] = vecs
      .tail
      .foldLeft(List(vecs.head))(Vec.vecFilter)

    reduced :+ reduced.head
  }

  private def vecs2polygon(vecs: List[Vec]): Polygon = {
    geometryFactory
      .createPolygon(vecs.map(_.coordinate).toArray)
      .norm
      .asInstanceOf[Polygon]
  }

  def removeColinearity: Polygon => Polygon = 
    polygon2vecs _ andThen filterVecs _ andThen vecs2polygon _
}


object OrthogonalPolygonBuilder {
  import GeometryUtils.IterablePolygon

  val tol: Double = scala.math.pow(2, -12)
  val geometryFactory = new GeometryFactory()

  def coverCoordinates(points: Iterable[Coordinate]): Geometry = {
    geometryFactory
      .createLineString(points.toArray)
      .getEnvelope
  }

  def build(polygon: Polygon, tolerance: Double = tol, size: Int = 3, step: Int = 1): Polygon = {
    val simpler: Polygon = DouglasPeuckerSimplifier
      .simplify(polygon, tolerance.max(0))
      .asInstanceOf[Polygon]

    val length: Int = size.max(3)
    val window: Int = step.min(length - 2).max(1)

    val coveringRectangles: List[Geometry] = simpler
      .sliding(length, window)
      .map(coverCoordinates)
      .toList

    val newBoundary: LineString = CascadedPolygonUnion
      .union(coveringRectangles.asJavaCollection)
      .asInstanceOf[Polygon]
      .getExteriorRing

    OrthogonalPolygonSimplifier removeColinearity
      geometryFactory.createPolygon(newBoundary.getCoordinates)
  }
}
