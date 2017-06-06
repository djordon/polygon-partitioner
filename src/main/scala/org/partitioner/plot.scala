package org.partitioner

import java.io.File

import com.vividsolutions.jts.geom.Polygon
import plotly.element.{Color, Line, Marker, ScatterMode}
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

  def extendedCornerPlotter(line: ExtendedCorner): Scatter = {
    val points: List[Point] = List(line.source, line.dest)

    scatterLines(
      points=points,
      markerColor=Color.RGBA(152, 0, 0, 0.8),
      markerLine=Line(color = Color.RGBA(30, 0, 0, 1.0), width = 1.0)
    )
  }

  def rectanglePlotter(rectangle: Rectangle): Scatter = {
    val points: List[Point] = List(rectangle.upperLeft, rectangle.lowerRight)

    scatterLines(
      points=points,
      markerColor=Color.RGBA(0, 152, 0, 0.8),
      markerLine=Line(color = Color.RGBA(0, 30, 0, 1.0), width = 1.0)
    )
  }

  def polygonPlotter(polygon: Polygon): Seq[Scatter] = {
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
      innerLines: List[ExtendedCorner] = Nil,
      diagLines: List[Rectangle] = Nil,
      plotName: String = "quick",
      fileName: String = "quick.html"): File = {

    val scatters: Seq[Scatter] = polygons.flatMap(polygonPlotter) ++
      innerLines.map(extendedCornerPlotter) ++
      diagLines.map(rectanglePlotter)

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
