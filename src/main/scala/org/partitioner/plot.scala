package org.partitioner

import java.io.File

import com.vividsolutions.jts.geom.Polygon

import plotly.element.{Color, Line, Marker, ScatterMode, Fill}
import plotly.layout.{Axis, Layout}
import plotly.{Plotly, Scatter}


object PolygonPlotter {
  import GeometryUtils.IterablePolygon

  def scatterLines(points: List[Point], markerColor: Color, markerLine: Line):
      Scatter = {
    Scatter(
      values=points.map(_.x),
      secondValues=points.map(_.y),
      mode=ScatterMode(ScatterMode.Markers, ScatterMode.Lines),
      marker=Marker(color = markerColor, line = markerLine)
    )
  }

  def extendedCornerPlotter(line: CornerLine): List[Scatter] = {
    val points: List[Point] = List(line.source, line.dest)

    List(scatterLines(
      points=points,
      markerColor=Color.RGBA(152, 0, 0, 0.8),
      markerLine=Line(color = Color.RGBA(30, 0, 0, 1.0), width = 1.0)
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
      mode = ScatterMode(ScatterMode.Markers, ScatterMode.Lines),
      marker = marker
    ) ::
    Scatter(
      values = top.map(_.x),
      secondValues = top.map(_.y),
      mode = ScatterMode(ScatterMode.Markers, ScatterMode.Lines),
      marker = marker,
      fill = Fill.ToNextY
    ) :: Nil
  }

  def polygonPlotter(polygon: Polygon): List[Scatter] = {
    val interior: List[List[Point]] = polygon.getHoles.map(_.toList.map(Point.apply))
    val exterior: List[Point] = polygon.toList.map(Point.apply)

    val scatterPartial = scatterLines(
      _: List[Point],
      markerColor=Color.RGBA(255, 153, 51, 0.8),
      markerLine=Line(color = Color.RGBA(255, 153, 51, 0.8), width = 1.0)
    )

    (exterior :: interior).map(scatterPartial)
  }

  def quickPlot(
                 polygons: List[Polygon],
                 innerLines: List[CornerLine] = Nil,
                 diagLines: List[Rectangle] = Nil,
                 plotName: String = "quick",
                 fileName: String = "quick.html"): File = {

    val scatters: List[Scatter] = polygons.flatMap(polygonPlotter) ++
      innerLines.flatMap(extendedCornerPlotter) ++
      diagLines.flatMap(rectanglePlotter)

    val layout = Layout(
      title = plotName,
      xaxis = Axis(showgrid=true),
      yaxis = Axis(showgrid=true),
      width = 600,
      height = 600
    )
    Plotly.plot(fileName, scatters, layout)
  }
}
