package org.jeom

import java.awt.Graphics2D
import java.awt.image.BufferedImage
import java.awt.geom.Path2D
import java.io.File
import javax.imageio.ImageIO

import com.vividsolutions.jts.geom.{GeometryCollection, GeometryFactory, Geometry, Polygon, Coordinate}


object GeometryUtils {
  implicit class IterableCollection(val gc: GeometryCollection) extends Iterable[Geometry] {
    override def iterator: Iterator[Geometry] = {
      for (i <- 0 until gc.getNumGeometries)
        yield gc.getGeometryN(i)
    }.toIterator
  }

  implicit class IterablePolygon(val pg: Polygon) extends Iterable[Coordinate] {
    override def iterator: Iterator[Coordinate] = pg.getCoordinates.toIterator
  }

  def emptyPolygon: Polygon = (new GeometryFactory()).createPolygon(Array[Coordinate]())

  def plot(polygon: Polygon, mapSize: Int = 800): Unit = {
    val minX = polygon.getCoordinates.map(_.x).min
    val minY = polygon.getCoordinates.map(_.x).min
    val xs = polygon.getCoordinates.map(_.x - minX)
    val ys = polygon.getCoordinates.map(_.y - minY)

    val img = new BufferedImage(mapSize, mapSize, BufferedImage.TYPE_INT_ARGB)
    val g = img.createGraphics()

    g.clearRect(0, 0, mapSize, mapSize)
    g.setColor(java.awt.Color.RED)
    // g.drawPolygon(xs, ys, xs.size)

    val outputFile = new File("/Users/dan/Documents/notes/test-image.png")
    ImageIO.write(img, "png", outputFile)
  }

  def plotly(
      polygon: Polygon,
      original: Option[Polygon] = None,
      plotName: String = "scatter-mode"): Unit = {
    import co.theasi.plotly._

    // if (!original.isEmpty) {
    val b = original.getOrElse(emptyPolygon).getCoordinates
    val xs0: Array[Double] = b.map(_.x)
    val ys0: Array[Double] = b.map(_.y)
    // } else {
    //   val xs0: Array[Double] = Array()
    //   val ys0: Array[Double] = Array()
    // }

    val xs1 = polygon.getCoordinates.map(_.x)
    val ys1 = polygon.getCoordinates.map(_.y)

    val p = Plot()
      .withScatter(xs0, ys0,
        ScatterOptions()
          .mode(ScatterMode.Marker, ScatterMode.Line)
          .name("ZCTA Polygon"))
      .withScatter(xs1, ys1,
        ScatterOptions()
          .mode(ScatterMode.Marker, ScatterMode.Line)
          .name("Orthogonal Polygon Cover"))

    draw(p, plotName, writer.FileOptions(overwrite=true))
  }
}

object NothingReally {
/*
val minX = polygon.getCoordinates.map(_.x).min
val minY = polygon.getCoordinates.map(_.x).min
val xs = polygon.getCoordinates.map(_.x - minX)
val ys = polygon.getCoordinates.map(_.y - minY)

val path = new Path2D.Double()

path.moveTo(xs.head, ys.head)

for ( (x, y) <- xs.tail zip ys.tail) {
  path.lineTo(x, y)
}
path.closePath()

val img = new BufferedImage(mapSize, mapSize, BufferedImage.TYPE_INT_ARGB)
val g = img.createGraphics()

g.setColor(java.awt.Color.WHITE)
g.fillRect(0, 0, img.getWidth, img.getHeight)
g.clearRect(xs.min.toInt - 1, ys.min.toInt - 1, xs.max.toInt + 1, ys.max.toInt + 1)
g.setColor(java.awt.Color.RED)
g.draw(path)
// g.dispose()

val outputFile = new File("/Users/dan/Documents/notes/test-image.png")

ImageIO.write(img, "png", outputFile)

import java.awt.{Point, Graphics}
import java.awt.event.{MouseAdapter, MouseEvent}
import javax.swing.{JPanel, JFrame}

trait Shape {
  def location: Point
  def location_=(p: Point): Unit
  def draw(g: Graphics): Unit
}

class Poly(val path2d: Path2D) extends Shape {
  def draw(g: Graphics) = g.asInstanceOf[Graphics2D].draw(path2d)
}

object ShapeTest {
  def main(args: Array[String]) {
    val shapes: List[Shape] = List(new Poly(path))

    val panel = new JPanel {
      override def paintComponent(g: Graphics) {
        super.paintComponent(g)
        for (shape <- shapes) shape.draw(g)
      }

      addMouseListener(new MouseAdapter {
        override def mouseClicked(e: MouseEvent) {
          for (shape <- shapes) shape.location = e.getPoint
          repaint()
        }
      })
    }

    val frame = new JFrame("Shape Test") {
      setSize(400, 300)
      add(panel)
    }

    frame.setVisible(true)
  }
}
*/
}
