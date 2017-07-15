package org.partitioner.orthogonal

import org.partitioner._

import scala.collection.immutable.TreeMap


object OrthogonalPolygonChordReducer {

  case class ChordContainer(
    openedCorners: TreeMap[Double, Corner] = TreeMap(),
    intersections: Map[Chord, Set[Corner]] = Map())

  def intersecting(treeMap: TreeMap[Double, Corner])(cn: Chord): (Chord, Set[Corner]) = {
    (cn, treeMap
      .from(cn.yMin)
      .to(cn.yMax)
      .values
      .toSet)
  }

  def setActions(corners: List[CornerGeometry], opened: TreeMap[Double, Corner])
      : Map[String, List[CornerGeometry]] = {

    val toOpenClose: Map[String, List[Corner]] = corners
      .collect { case c: Corner => c }
      .groupBy(cn => cn.point)
      .filter(_._2.length == 1)
      .map(_._2.head)
      .toList
      .groupBy(cn => if (opened contains cn.y) "toClose" else "toOpen")

    toOpenClose + ("toIntersect" -> corners.collect { case ch: Chord => ch })
  }

  private def lineSweeper(container: ChordContainer, cgs: List[CornerGeometry])
      : ChordContainer = {

    val actions = setActions(cgs, container.openedCorners)

    val opened: TreeMap[Double, Corner] = container.openedCorners ++
      actions
        .getOrElse("toOpen", Nil)
        .collect { case cn: Corner => (cn.y, cn) }
        .toMap

    val closed: TreeMap[Double, Corner] = opened --
      actions.getOrElse("toClose", Nil).map(_.y)

    val intersections: Map[Chord, Set[Corner]] = actions
      .getOrElse("toIntersect", Nil)
      .collect { case ch: Chord => intersecting(opened)(ch) }
      .toMap

    ChordContainer(closed, intersections ++ container.intersections)
  }

  def computeIntersections(chords: List[Chord]): List[(Chord, Chord)] = {
    val horizontalChords: List[CornerGeometry] = chords.flatMap {
      ch => if (ch.pointsVertically) List(ch) else ch.toListCorner
    }

    val lineContainer: ChordContainer = horizontalChords
      .groupBy(_.x)
      .toList
      .sortBy(_._1)
      .map(_._2)
      .foldLeft(ChordContainer())(lineSweeper)

    val chordMap: Map[Corner, Chord] = chords
      .filterNot(_.pointsVertically)
      .map(ch => (ch.left, ch))
      .toMap

    lineContainer.intersections
      .mapValues(cns => cns.map(chordMap(_)))
      .toList
      .flatMap { kv => kv._2.map { (_, kv._1) } }
  }

  def removeDuplicateChords(chords: List[Chord]): List[Chord] = chords
    .foldLeft(Set[Chord]()) { (set, ch) => if (set contains ch.swap) set else set + ch }
    .toList

  def reduceChords(chords: List[Chord]): List[Chord] = {
    val distinctChords: List[Chord] = removeDuplicateChords(chords)
    val intersections: List[(Chord, Chord)] = computeIntersections(distinctChords)

    val all: Set[Chord] = intersections.flatMap(t => List(t._1, t._2)).toSet
    val chordsL: List[Chord] = intersections.map(_._1).distinct
    val chordsR: List[Chord] = intersections.map(_._2).distinct

    if (chordsL.length > chordsR.length)
      chordsL ::: distinctChords.filterNot(all.contains(_))
    else
      chordsR ::: distinctChords.filterNot(all.contains(_))
  }
}
