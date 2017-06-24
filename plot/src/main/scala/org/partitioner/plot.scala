package org.partitioner.plot

import java.io.File

import com.vividsolutions.jts.geom.Polygon

import org.partitioner._

import plotly.element._
import plotly.layout.{Axis, Layout, HoverMode, Margin}
import plotly.{Plotly, Scatter}


trait PlotDefaults {
  lazy val boundaryLine = Line(color = boundaryMarkerColor, width = 1.0)
  lazy val boundaryMarkerColor = Color.RGBA(194, 33, 10, 0.9)
  lazy val boundaryMarker = Marker(color = boundaryMarkerColor, line = boundaryLine)

  val defaultLayout = Layout(
    title = "",
    xaxis = Axis(showgrid = true, showticklabels = false, zeroline = false),
    yaxis = Axis(showgrid = true, showticklabels = false, zeroline = false),
    width = 550,
    height = 550,
    showlegend = false,
    plot_bgcolor = Color.RGBA(0, 0, 0, 0),
    paper_bgcolor = Color.RGBA(0, 0, 0, 0),
    hovermode = HoverMode.Closest,
    margin = Margin(l = 15, r = 15, t = 15, b = 15)
  )

  lazy val interiorLine = Line(
    color = interiorMarkerColor,
    width = 1.0,
    dash = Dash.Dot
  )
  lazy val interiorMarkerColor = Color.RGBA(175, 175, 175, 0.85)
  lazy val interiorLineMarker = Marker(
    color = interiorMarkerColor,
    line = interiorLine
  )
}


object PolygonPlotter extends PlotDefaults {
  import GeometryUtils.IterablePolygon
  import plotly.Sequence.Doubles

  def pointScatter(
      points: List[Point],
      marker: Marker,
      text: Option[Seq[String]] = None,
      mode: Option[ScatterMode] = Some(ScatterMode(ScatterMode.Lines)),
      line: Option[Line] = None,
      textPosition: Option[TextPosition] = None,
      textFont: Option[TextFont] = None,
      name: Option[String] = None,
      connectGaps: Option[Boolean] = None,
      xAxis: Option[AxisReference] = None,
      yAxis: Option[AxisReference] = None,
      fill: Option[Fill]= None): Scatter = {

    Scatter(
      x = Some(Doubles(points.map(_.x))),
      y = Some(Doubles(points.map(_.y))),
      mode = mode,
      marker = Some(marker),
      text = text,
      line = line,
      textposition = textPosition,
      textfont = textFont,
      name = name,
      connectgaps = connectGaps,
      xaxis = xAxis,
      yaxis = yAxis,
      fill = fill,
      error_x = Option(null),
      error_y = Option(null)
    )
  }

  def cornerLinePlotter(
      line: CornerLine,
      marker: Marker = interiorLineMarker): List[Scatter] = {

    val points: List[Point] = List(line.source, line.dest)

    List(pointScatter(points, marker = marker))
  }

  def rectanglePlotter(
      rectangle: Rectangle,
      shadeColor: Option[Color] = None): List[Scatter] = {

    val top: List[Point] = List(rectangle.upperLeft, rectangle.upperRight)
    val bot: List[Point] = List(rectangle.lowerLeft, rectangle.lowerRight)

    val color: Color = shadeColor getOrElse {
      Color.RGBA(0, 15, (Math.random() * 200).toInt + 55, 0.7)
    }
    val marker: Marker = Marker(
      color = color,
      line = Line(color = color, width = 1.0)
    )

    List(
      pointScatter(bot, marker),
      pointScatter(top, marker, fill = Some(Fill.ToNextY))
    )
  }

  def polygonPlotter(
      polygon: Polygon,
      marker: Marker = boundaryMarker): List[Scatter] = {

    val interior: List[List[Point]] = polygon.getHoles.map(_.toList.map(Point.apply))
    val exterior: List[Point] = polygon.toList.map(Point.apply)

    val partialScatter = pointScatter(
      _: List[Point],
      marker,
      mode = Some(ScatterMode(ScatterMode.Markers, ScatterMode.Lines))
    )

    (exterior :: interior).map(partialScatter)
  }

  def quickPlot(
      polygons: List[Polygon],
      innerLines: List[CornerLine] = Nil,
      rectangles: List[Rectangle] = Nil,
      title: String = "",
      fileName: String = "quick.html",
      layout: Layout = defaultLayout,
      rectangleColor: Option[Color] = None,
      polygonMarker: Marker = boundaryMarker,
      interiorMarker: Marker = interiorLineMarker): File = {

    val scatters: List[Scatter] = {
      rectangles.flatMap(rectanglePlotter(_: Rectangle, rectangleColor)) ++
      innerLines.flatMap(cornerLinePlotter(_: CornerLine, interiorMarker)) ++
      polygons.flatMap(polygonPlotter(_: Polygon, polygonMarker))
    }

    Plotly.plot(fileName, scatters, layout.copy(title = Some(title)))
  }
}
