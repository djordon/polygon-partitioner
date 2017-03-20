package org.jeom

import scala.collection.JavaConverters._

import com.vividsolutions.jts.simplify.DouglasPeuckerSimplifier
import com.vividsolutions.jts.triangulate.DelaunayTriangulationBuilder
import com.vividsolutions.jts.geom.{GeometryFactory, GeometryCollection, Geometry, Polygon, Coordinate}
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


object OthogonalPolygonBuilder {
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

  def polygon2vecs(pg: Polygon): List[Vec] = {
    pg.toList
      .sliding(2, 1)
      .map(Vec.vecConstructor)
      .toList
  }

  def filterVecs(vecs: List[Vec]): List[Vec] = {
    val reduced: List[Vec] = vecs
      .tail
      .foldLeft(List(vecs.head))(Vec.vecFilter)

    reduced :+ reduced.head
  }

  def vecs2polygon(vecs: List[Vec]): Polygon = {
    polygon = geometryFactory.createPolygon(vecs.map(_.coordinate).toArray)
    polygon.normalize()
    polygon
  }

  def reduceColinearity: Polygon => Polygon = 
    polygon2vecs _ andThen filterVecs _ andThen vecs2polygon _ 

}
