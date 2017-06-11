package org.partitioner.partition

import org.partitioner._

import scala.collection.immutable.TreeMap


object OrthogonalPolygonChordReducer {

  case class ChordContainer(
    openedCorners: TreeMap[Double, Corner] = TreeMap(),
    intersections: Map[Chord, Set[Corner]] = Map())

  def intersecting(treeMap: TreeMap[Double, Corner])(cn: Chord): Tuple2[Chord, Set[Corner]] = {
    //    println(cn.source.y)
    //    println(cn.dest.y)
    //    println(treeMap)
    //    println
    (cn, treeMap
      .from(cn.ymin)
      .to(cn.ymax)
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

    toOpenClose + ("toExtend" -> corners.collect { case ch: Chord => ch })
  }

  private def lineSweeper(container: ChordContainer, cgs: List[CornerGeometry])
  : ChordContainer = {

    val actions = setActions(cgs, container.openedCorners)

    val opened: TreeMap[Double, Corner] = container.openedCorners ++
      actions
        .getOrElse("toOpen", Nil)
        .map(cn => (cn.y, cn))
        .toMap
        .asInstanceOf[Map[Double, Corner]]

    val closed: TreeMap[Double, Corner] = opened --
      actions.getOrElse("toClose", Nil).map(_.y)

    val intersections: Map[Chord, Set[Corner]] = actions
      .getOrElse("toExtend", Nil)
      .asInstanceOf[List[Chord]]
      .map(intersecting(opened))
      .toMap
    //    println(opened)
    //    println(closed)
    //    println(actions.getOrElse("toExtend", Nil))
    //    println(intersections)
    ChordContainer(closed, intersections ++ container.intersections)
  }

  def computeIntersections(chords: List[Chord]): List[Tuple2[Chord, Chord]] = {
    val horizontalChords: List[CornerGeometry] = chords.flatMap { ch =>
      if (ch.angle.abs == 90) List(ch) else ch.toListCorner
    }

    val lineContainer: ChordContainer = horizontalChords
      .groupBy(_.x)
      .toList
      .sortBy(_._1)
      .map(_._2)
      .foldLeft(ChordContainer())(lineSweeper)

    val chordMap: Map[Corner, Chord] = chords
      .filter(_.angle.abs != 90)
      .map(ch => (ch.left, ch))
      .toMap
    //    println(chords)
    //    println(horizontalChords)
    //    println(horizontalChords
    //      .groupBy(_.x)
    //      .toList
    //      .sortBy(_._1)
    //      .map(_._2))
    //    println(lineContainer.intersections)
    //    println(chordMap)
    lineContainer.intersections
      .mapValues(cns => cns.map(chordMap(_)))
      .flatMap { kv => kv._2 zip Stream.continually(kv._1) }
      .toList
  }

  def reduceChords(chords: List[Chord]): List[Chord] = {
    val intersections: List[Tuple2[Chord, Chord]] = computeIntersections(chords)

    val all: Set[Chord] = intersections.flatMap(t => List(t._1, t._2)).toSet
    val chordsL: List[Chord] = intersections.map(_._1).distinct
    val chordsR: List[Chord] = intersections.map(_._2).distinct

    //    println(all)
    //    println(chordsL)
    //    println(chordsR)
    if (chordsL.length > chordsR.length)
      chordsL ::: chords.filterNot(all.contains(_))
    else
      chordsR ::: chords.filterNot(all.contains(_))
  }
}