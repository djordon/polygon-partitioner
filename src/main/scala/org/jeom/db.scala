package org.jeom

import doobie.imports._
import scalaz.concurrent.Task
import scala.collection.JavaConversions._


object PolygonExtractor {
  def extractZCTAs(maxPolys: Int = 50): List[MultiPolygonZip] = {
    val sysUser = sys.env.get("USER").get
    val hostname = sys.env.getOrElse("POSTGIS_HOST", "localhost")
    val username = sys.env.getOrElse("POSTGIS_USERNAME", sysUser)
    val password = sys.env.getOrElse("POSTGIS_PASSWORD", "")
    val dbname = sys.env.getOrElse("POSTGIS_DATABASE", "")
    val schema = sys.env.getOrElse("POSTGIS_SCHEMA", "public")
    val post = sys.env.getOrElse("POSTGIS_SCHEMA", 5432)

    val xa = DriverManagerTransactor[Task](
      "org.postgresql.Driver",
      s"jdbc:postgresql:$dbname", 
      username,
      password
    )

    sql"""
      SELECT
        geoid10 -- zip code
        , ST_AsText(geom)  -- multi polygon
      FROM tiger.zcta
      ORDER BY geoid10
      LIMIT $maxPolys;"""
      .query[(String, String)]
      .list
      .transact(xa)
      .unsafePerformSync
      .map(Function.tupled(MultiPolygonZip.apply _))
  }
}
