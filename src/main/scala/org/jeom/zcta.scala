package org.jeom

import com.vividsolutions.jts.triangulate.DelaunayTriangulationBuilder
import com.vividsolutions.jts.simplify.DouglasPeuckerSimplifier
import com.vividsolutions.jts.geom.{MultiPolygon, GeometryFactory}
import com.vividsolutions.jts.io.WKTReader


case class MultiPolygonZip(zipcode: String, multipolygon: MultiPolygon) {}


object MultiPolygonZip {
  def apply(zc: String, wkt: String) = {
    val wktReader = new WKTReader()
    new MultiPolygonZip(zc, wktReader.read(wkt).asInstanceOf[MultiPolygon])
  }
}
