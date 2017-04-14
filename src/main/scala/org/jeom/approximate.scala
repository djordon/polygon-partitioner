package org.jeom

import scala.collection.JavaConverters._

import com.vividsolutions.jts.algorithm.Angle
import com.vividsolutions.jts.densify.Densifier
import com.vividsolutions.jts.simplify.DouglasPeuckerSimplifier
import com.vividsolutions.jts.triangulate.DelaunayTriangulationBuilder
import com.vividsolutions.jts.geom.{GeometryFactory, Geometry, Polygon, Coordinate, LineString}
import com.vividsolutions.jts.operation.union.CascadedPolygonUnion


class OrthogonalPolygon(polygon: Polygon) {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")
  }
}


case class Vec(coord: Coordinate, angle: Double)


object Vec {
  def apply(a: List[Coordinate]) = 
    new Vec(a(1), Angle.toDegrees(Angle.angle(a.head, a.tail.head)))
}


object PolygonApproximator {
  import GeometryUtils.IterablePolygon
  val geometryFactory = new GeometryFactory()
  val angles: Set[Double] = Set(0.0, 90.0, 180.0, -90.0, 270.0)
  val epsilon: Double = 1.1102230246251568E-8

  def polygon2vecs(pg: Polygon): List[Vec] = {
    val boundary: List[Coordinate] = pg.toList
    // Stream
    //   .continually(boundary)
    //   .flatten
    //   .take(boundary.length + 3)
    pg.toList
      .sliding(2, 1)
      .map(Vec.apply)
      .toList
  }

  private def coordinates2vecs(coords: Iterable[Coordinate]): List[Vec] = {
    val boundary: List[Coordinate] = coords.toList
    Stream
      .continually(boundary)
      .flatten
      .take(boundary.length * 2)
      .toList
      .sliding(2, 1)
      .map(Vec.apply)
      .toList
  }

  private def vecBasisFolder(a: List[Vec], b: Vec): List[Vec] = {
    if ((a.head.angle - b.angle).abs < epsilon && angles.contains(b.angle)) 
      b :: a.tail
    else
      b :: a
  }

  private def filterVecs(folder: (List[Vec], Vec) => List[Vec])(vecs: List[Vec])
    : List[Vec] = {

    val reduced: List[Vec] = vecs
      .drop(2)
      .foldLeft(vecs.take(2))(folder)

    val boundary = reduced.take(reduced.length / 2 + 1).tail
      // .foldLeft(reduced.last :: reduced.head :: Nil)(folder)
    val last = boundary.last
    if (last == boundary.head) boundary else last :: boundary
  }

  private def isAxisAligned(a: List[Vec]): Boolean = {
    (a(0).coord.x == a(2).coord.x && a(1).coord.x == a(2).coord.x) ||
    (a(0).coord.y == a(2).coord.y && a(1).coord.y == a(2).coord.y)
  }

  private def rectilinearFolder(a: List[Vec], b: Vec): List[Vec] = {
    if (isAxisAligned { b :: a.take(2) }) b :: a.tail else b :: a
  }

  def vecs2polygon(vecs: List[Vec]): Polygon = {
    geometryFactory
      .createPolygon(vecs.map(_.coord).toArray)
      .norm
      .asInstanceOf[Polygon]
  }

  private def partialSimplify: Polygon => Polygon = simplify(_: Polygon, epsilon)

  def removeColinearity: Polygon => Polygon = partialSimplify andThen partialSimplify

  def removeAxisAlignedColinearity: Iterable[Coordinate] => List[Vec] = {//Polygon = {
    // polygon2vecs _ andThen 
    coordinates2vecs _ andThen 
    filterVecs(rectilinearFolder) _ //andThen 
    // vecs2polygon _
  }

  // def removeAxisAlignedColinearity: Polygon => Polygon =
  //   reduceAxisAlignedColinearity andThen reduceAxisAlignedColinearity

  // def removeRectilinearColinearity: Polygon => Polygon = {
  //   polygon2vecs _ andThen 
  //   filterVecs(rectilinearFolder) _ andThen 
  //   vecs2polygon _
  // }

//partialSimplify andThen partialSimplify

  def simplify(polygon: Polygon, tolerance: Double): Polygon = DouglasPeuckerSimplifier
    .simplify(polygon, tolerance)
    .norm
    .asInstanceOf[Polygon]

  def densify(polygon: Polygon, tolerance: Double): Polygon = Densifier
    .densify(polygon, tolerance)
    .norm
    .asInstanceOf[Polygon]
}


object OrthogonalPolygonBuilder {
  import GeometryUtils.IterablePolygon
  import PolygonApproximator.{densify, removeColinearity, simplify}

  val tol: Double = scala.math.pow(2, -12)
  val geometryFactory = new GeometryFactory()

  def coverCoordinates(points: Iterable[Coordinate]): Geometry = {
    geometryFactory
      .createLineString(points.toArray)
      .getEnvelope
  }

  def cover(polygon: Polygon, size: Int = 3, step: Int = 1): List[Coordinate] = {
    val simpler: List[Coordinate] = PolygonApproximator.removeAxisAlignedColinearity(polygon).map(_.coord)
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

    newBoundary.getCoordinates.toList
    // geometryFactory.createPolygon(newBoundary.getCoordinates)
    // PolygonApproximator
      // .removeAxisAlignedColinearity(geometryFactory.createPolygon(newBoundary.getCoordinates))
  }

  def approximate(
      polygon: Polygon,
      simplifyTolerance: Double = tol,
      densifyTolerance: Double = 10.0,
      size: Int = 3,
      step: Int = 1): Polygon = {

    // val method: Polygon => Polygon = Function.chain(Seq(
    //   simplify(_: Polygon, simplifyTolerance),
    //   densify(_: Polygon, densifyTolerance),
    //   cover(_: Polygon, size, step)
    // ))

    // method(polygon)
    polygon
  }
}
