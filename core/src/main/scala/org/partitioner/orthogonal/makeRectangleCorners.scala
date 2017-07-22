package org.partitioner.orthogonal

import org.partitioner.{Chord, Corner, CornerGeometry, CornerLine, Point}


private[partitioner] object createInteriorLines
  extends Function1[List[List[Corner]], List[CornerGeometry]] {

  import OrthogonalPolygonCornerExtender.extendCorners

  def apply(corners: List[List[Corner]]): List[CornerGeometry] = {

    val co: List[Corner] = corners.flatMap(_.tail)
    extendCorners(co)(true) ::: extendCorners(co)(false)
  }
}


private[partitioner] object createChordsCornerLines
  extends Function1[List[List[Corner]], (List[Chord], List[CornerLine])] {

  def separateChordsCornerLines(
      interiorLines: List[CornerGeometry]): (List[Chord], List[CornerLine]) = {

    val chords: List[Chord] = OrthogonalPolygonChordReducer.reduceChords {
      interiorLines collect { case c: Chord => c }
    }

    val chordPoints: Set[Point] = chords
      .flatMap(_.toListCorner)
      .map(_.point)
      .toSet

    val lines: List[CornerLine] = interiorLines collect {
      case ec: CornerLine if !chordPoints.contains(ec.source) => ec
      case ch: Chord if !(chordPoints contains ch.point) => ch.toCornerLine
    }

    (chords, lines)
  }

  def apply(corners: List[List[Corner]]): (List[Chord], List[CornerLine]) = {
    createInteriorLines.andThen(separateChordsCornerLines)(corners)
  }
}


private[partitioner] object extractChordCorners
  extends Function2[List[Chord], Boolean, List[Corner]] {

  def apply(chords: List[Chord], vertical: Boolean): List[Corner] = chords
    .filter(_.pointsVertically == vertical)
    .flatMap(_.toListCorner)
    .map(_.copy(isConcave = false))
}



private[partitioner] object makeRectangleCorners
  extends Function1[List[List[Corner]], List[CornerGeometry]] {

  def apply(corners: List[List[Corner]]): List[CornerGeometry] = {

    val (chords, lines) = createChordsCornerLines(corners)

    val horizontalLines: List[CornerLine] = CornerLineAdjuster.adjustCornerGeometries {
      lines.filterNot(_.pointsVertically) ::: extractChordCorners(chords, true)
    } { false }

    val verticalLines: List[CornerLine] = CornerLineAdjuster.adjustCornerGeometries {
      horizontalLines.flatMap(_.toListCorner) :::
        lines.filter(_.pointsVertically) :::
        extractChordCorners(chords, false)
    } { true }

    val convexCorners = corners.flatMap(_.tail).filterNot(_.isConcave)

    chords ::: verticalLines ::: horizontalLines ::: convexCorners
  }
}
