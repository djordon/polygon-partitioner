package org.partitioner.orthogonal

import org.partitioner.{Chord, Corner, CornerGeometry, CornerLine, Point}

import scala.language.implicitConversions


case class EndpointStacks(
    upperLeft: List[Point] = Nil,
    lowerLeft: List[Point] = Nil,
    lowerRight: List[Point] = Nil) {

  def prepend(ul: List[Point] = Nil, ll: List[Point] = Nil, lr: List[Point] = Nil) =
    EndpointStacks(ul ::: upperLeft, ll ::: lowerLeft, lr ::: lowerRight)
}


private[orthogonal] trait RectangleEndpointExtractor {
  private[this] implicit def pointToListPoint(pt: Point): List[Point] = List(pt)

  private[this] def stackExtendedCorner(ex: CornerLine, stacks: EndpointStacks): EndpointStacks = {
    ex match {
      case CornerLine(s, d, 0) => stacks.prepend(ll=s, lr=d)
      case CornerLine(s, d, 90) => stacks.prepend(ul=d, lr=s)
      case CornerLine(s, d, 180) => stacks.prepend(ul=d, ll=d)
      case CornerLine(s, d, -90) => stacks.prepend(ul=s, ll=d, lr=d)
      case _ => stacks
    }
  }

  private[this] def stackCorner(cn: Corner, stacks: EndpointStacks): EndpointStacks = {
    cn match {
      case Corner(s, false, 90) => stacks.prepend(ul=s)
      case Corner(s, false, 180) => stacks.prepend(ll=s)
      case Corner(s, false, -90) => stacks.prepend(lr=s)
      case _ => stacks
    }
  }

  private[this] def stackChord(ch: Chord, stacks: EndpointStacks): EndpointStacks = {
    ch match {
      case Chord(Corner(s, _, 90), Corner(d, _, 0)) => stacks.prepend(lr=s)
      case Chord(Corner(s, _, 90), Corner(d, _, -90)) => stacks.prepend(lr=s, ul=d)
      case Chord(Corner(s, _, -90), Corner(d, _, 90)) => stacks.prepend(lr=d, ul=s)
      case Chord(Corner(s, _, -90), Corner(d, _, 180)) => stacks.prepend(ll=d, ul=s)
      case Chord(Corner(s, _, 180), Corner(d, _, 90)) => stacks.prepend(ul=d)
      case Chord(Corner(s, _, 180), Corner(d, _, 0)) => stacks.prepend(ll=d)
      case Chord(Corner(s, _, 0), Corner(d, _, 180)) => stacks.prepend(ll=s)
      case Chord(Corner(s, _, 0), Corner(d, _, -90)) => stacks.prepend(ll=s, lr=d)
      case _ => stacks
    }
  }

  private[this] def cornerFolder(
      stacks: EndpointStacks,
      corner: CornerGeometry): EndpointStacks = {

    corner match {
      case ex: CornerLine => stackExtendedCorner(ex, stacks)
      case cn: Corner => stackCorner(cn, stacks)
      case ch: Chord => stackChord(ch, stacks)
    }
  }

  def extractRectangleEndpoints(
      cornersPoints: List[CornerGeometry]): EndpointStacks = {

    cornersPoints.foldLeft(EndpointStacks())(cornerFolder)
  }
}