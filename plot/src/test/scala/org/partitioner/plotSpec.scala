package org.partitioner.plot

import org.partitioner._

import org.scalatest.{Matchers, WordSpec}

import scala.language.reflectiveCalls



class PolygonPlotterSpec extends WordSpec with Matchers with PolygonFixtures {
  "PolygonPlotter" can {
    "polygonPlotter" should {
      "return a Scatter for the exterior and holes of the polygon" in {
        for (pg <- fixtures.values) {
          val numGeometries: Int = pg.getNumGeometries
          val scatters = PolygonPlotter.polygonPlotter(pg)

          scatters.length shouldEqual numGeometries
        }
      }
    }

    "rectanglePlotter" should {
      "return a Scatter for the top and bottom lines of a rectangle" in {
        val rectangle: Rectangle = Rectangle(Point(0, 1), Point(1, 0))
        PolygonPlotter.rectanglePlotter(rectangle).length shouldEqual 2
      }
    }

    "cornerLinePlotter" should {
      "return one Scatter for the line" in {
        val line: CornerLine = CornerLine(Point(0, 0), Point(0, 1), 90)
        PolygonPlotter.cornerLinePlotter(line).length shouldEqual 1
      }
    }
  }
}