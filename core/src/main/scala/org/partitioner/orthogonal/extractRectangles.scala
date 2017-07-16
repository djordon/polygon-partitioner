package org.partitioner.orthogonal

import org.partitioner._

import scala.collection.Searching.{Found, InsertionPoint, SearchResult, search}
import scala.language.implicitConversions


object extractRectangles extends Function1[List[CornerGeometry], List[Rectangle]]
                            with RectangleEndpointExtractor {

  private[this] def extractIndex(sr: SearchResult): Int = sr match {
    case Found(i) => i + 1
    case InsertionPoint(i) => i
  }

  def apply(cornersPoints: List[CornerGeometry]): List[Rectangle] = {
    val (upperLeft, lowerLeft, lowerRight) = extractRectangleEndpoints(cornersPoints) match {
      case EndpointStacks(ul, ll, lr) => (ul, ll, lr)
    }
    val ul: Vector[Point] = upperLeft.toVector.sorted(PointOrderingX)
    val lr: Vector[Point] = lowerRight.toVector.sorted(PointOrderingY)

    lowerLeft map { p =>
      val i1 = extractIndex(ul.search(p)(PointOrderingX))
      val i2 = extractIndex(lr.search(p)(PointOrderingY))
      Rectangle(ul(i1), lr(i2))
    }
  }
}