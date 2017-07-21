package org.partitioner.db

import com.vividsolutions.jts.io.WKTReader
import com.vividsolutions.jts.geom.MultiPolygon

import doobie.imports._
import scalaz.concurrent.Task


case class MultiPolygonZip(zipcode: String, multipolygon: MultiPolygon)


object MultiPolygonZip {
  def apply(zc: String, wkt: String) = {
    val wktReader = new WKTReader()
    new MultiPolygonZip(zc, wktReader.read(wkt).asInstanceOf[MultiPolygon])
  }
}


object PolygonExtractor {

  def extractZCTAs(numPolygons: Int = 1): List[MultiPolygonZip] = {
    val sysUser = sys.env.get("USER").get
    val hostname = sys.env.getOrElse("POSTGIS_HOST", "localhost")
    val username = sys.env.getOrElse("POSTGIS_USERNAME", sysUser)
    val password = sys.env.getOrElse("POSTGIS_PASSWORD", "")
    val database = sys.env.getOrElse("POSTGIS_DATABASE", "")
    val schema = sys.env.getOrElse("POSTGIS_SCHEMA", "public")
    val post = sys.env.getOrElse("POSTGIS_SCHEMA", 5432)

    val xa = DriverManagerTransactor[Task](
      "org.postgresql.Driver",
      s"jdbc:postgresql:$database",
      username,
      password
    )

    sql"""
      SELECT
        geoid10  -- zip code
        , ST_AsText(geom)  -- multi polygon
      FROM tiger.zcta
      ORDER BY geoid10
      LIMIT $numPolygons;"""
      .query[(String,  String)]
      .list
      .transact(xa)
      .unsafePerformSync
      .map(Function.tupled(MultiPolygonZip.apply _))
  }
}
