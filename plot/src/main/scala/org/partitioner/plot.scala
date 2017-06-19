package org.partitioner.plot

import java.io.File

import com.vividsolutions.jts.geom.Polygon

import org.partitioner._

import plotly.element.{Color, Line, Marker, ScatterMode, Fill, Dash}
import plotly.layout.{Axis, Layout}
import plotly.{Plotly, Scatter}


object PolygonPlotter {
  import GeometryUtils.IterablePolygon

  def scatterLines(points: List[Point], markerColor: Color, markerLine: Line, mode: Option[ScatterMode] = None):
      Scatter = {
    Scatter(
      values = points.map(_.x),
      secondValues = points.map(_.y),
      mode = mode getOrElse ScatterMode(ScatterMode.Markers, ScatterMode.Lines),
      marker = Marker(color = markerColor, line = markerLine)
    )
  }

  def cornerLinePlotter(line: CornerLine): List[Scatter] = {
    val points: List[Point] = List(line.source, line.dest)

    List(scatterLines(
      points = points,
      markerColor = Color.RGBA(175, 175, 175, 0.85),
      mode = Some(ScatterMode(ScatterMode.Lines)),
      markerLine = Line(
        color = Color.RGBA(175, 175, 175, 0.95),
        width = 1.0,
        dash = Dash.Dot
      )
    ))
  }

  def rectanglePlotter(rectangle: Rectangle): List[Scatter] = {

    val top: List[Point] = List(rectangle.upperLeft, rectangle.upperRight)
    val bot: List[Point] = List(rectangle.lowerLeft, rectangle.lowerRight)

    val blueShade: Int = (Math.random() * 200).toInt + 55
    val blue: Color = Color.RGBA(0, 15, blueShade, 0.7)
    val marker: Marker = Marker(color = blue, line = Line(color = blue, width = 1.0))

    Scatter(
      values = bot.map(_.x),
      secondValues = bot.map(_.y),
      mode = ScatterMode(ScatterMode.Lines),
      marker = marker
    ) ::
    Scatter(
      values = top.map(_.x),
      secondValues = top.map(_.y),
      mode = ScatterMode(ScatterMode.Lines),
      marker = marker,
      fill = Fill.ToNextY
    ) :: Nil
  }

  def polygonPlotter(polygon: Polygon): List[Scatter] = {
    val interior: List[List[Point]] = polygon.getHoles.map(_.toList.map(Point.apply))
    val exterior: List[Point] = polygon.toList.map(Point.apply)

    val scatterPartial = scatterLines(
      _: List[Point],
      markerColor = Color.RGBA(194, 33, 10, 0.9),
      markerLine = Line(color = Color.RGBA(194, 33, 10, 0.9), width = 1.0)
    )

    (exterior :: interior).map(scatterPartial)
  }

  def quickPlot(
      polygons: List[Polygon],
      innerLines: List[CornerLine] = Nil,
      rectangles: List[Rectangle] = Nil,
      plotName: Option[String] = Some("quick"),
      fileName: String = "quick.html",
      plotLayout: Option[Layout] = None): File = {

    val scatters: List[Scatter] = {
      rectangles.flatMap(rectanglePlotter) ++
      innerLines.flatMap(cornerLinePlotter) ++
      polygons.flatMap(polygonPlotter)
    }

    val layout = Layout(
      title = plotName.getOrElse(""),
      xaxis = Axis(showgrid = true, showticklabels = false, zeroline = false),
      yaxis = Axis(showgrid = true, showticklabels = false, zeroline = false),
      width = 600,
      height = 600,
      showlegend = false,
      plot_bgcolor = Color.RGBA(0, 0, 0, 0),
      paper_bgcolor = Color.RGBA(0, 0, 0, 0)
    )

    Plotly.plot(fileName, scatters, plotLayout.getOrElse(layout))
  }
}
