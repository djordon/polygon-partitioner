package org.partitioner.plot

import java.io.File

import org.locationtech.jts.geom.Polygon
import org.partitioner._
import plotly.element._
import plotly.layout.{Axis, HoverMode, Layout, Margin}
import plotly.{Plotly, Scatter}


trait PlotDefaults {
  lazy val boundaryLine = Line(color = boundaryMarkerColor, width = 1.0)
  lazy val boundaryMarkerColor = Color.RGBA(194, 33, 10, 0.9)
  lazy val boundaryMarker = Marker(color = boundaryMarkerColor, line = boundaryLine)

  lazy val backgroundMarker: Marker = Marker(
    color = Color.RGB(0, 0, 0),
    line = Line(color = Color.RGB(0, 0, 0), width = 0.0)
  )

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

  def defaultRectangleColor(rec: Rectangle): Color = {
    Color.RGB(0, 15, (Math.random() * 200).toInt + 55)
  }
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
      rectangleColor: Rectangle => Color = defaultRectangleColor _): List[Scatter] = {

    val top: List[Point] = List(rectangle.upperLeft, rectangle.upperRight)
    val bot: List[Point] = List(rectangle.lowerLeft, rectangle.lowerRight)

    val color: Color = rectangleColor(rectangle)
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
      marker: Marker = boundaryMarker,
      fill: Option[Fill] = None): List[Scatter] = {

    val interior: List[List[Point]] = polygon.getHolesCoordinates.map(_.map(Point.apply))
    val exterior: List[Point] = polygon.toList.map(Point.apply)

    val partialScatter = pointScatter(
      _: List[Point],
      marker,
      mode = Some(ScatterMode(ScatterMode.Markers, ScatterMode.Lines)),
      fill = fill
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
      rectangleColor: Rectangle => Color = defaultRectangleColor _,
      polygonMarker: Marker = boundaryMarker,
      interiorMarker: Marker = interiorLineMarker,
      backgroundMarker: Marker = backgroundMarker,
      openInBrowser: Boolean = true): File = {

    val fill: Option[Fill] = Some(Fill.ToNextY)
    val scatters: List[Scatter] = {
      polygons.flatMap(polygonPlotter(_: Polygon, backgroundMarker, fill)) ++
      rectangles.flatMap(rectanglePlotter(_: Rectangle, rectangleColor)) ++
      innerLines.flatMap(cornerLinePlotter(_: CornerLine, interiorMarker)) ++
      polygons.flatMap(polygonPlotter(_: Polygon, polygonMarker))
    }

    Plotly.plot(fileName, scatters, layout.copy(title = Some(title)), openInBrowser = openInBrowser)
  }
}
