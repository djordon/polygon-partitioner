package org.partitioner

import org.partitioner.orthogonal._
import org.scalatest.WordSpec

import scala.util.Random


class GeometriesSpec extends WordSpec with PolygonFixtures {
  def createCornerLine: CornerLine = {
    val p1 = Point(Math.random(), Math.random())
    val p2 = Point(Math.random(), Math.random())

    CornerLine(p1, p2, Random.nextInt)
  }

  def createChord: Chord = {
    val p1 = Point(Math.random(), Math.random())
    val p2 = Point(Math.random(), Math.random())

    Chord(Corner(p1, Math.random > 0.5, Random.nextInt), Corner(p2, Math.random > 0.5, Random.nextInt))
  }

  "Corner" should {
    "toListCorner should contain one element" in {
      for (pg <- fixtures.values) {
        val corners = extractCorners(pg)
        assert { corners.flatten.map(_.toListCorner.length).toSet == Set(1) }
      }
    }

    "toListCorner should contain itself" in {
      for (pg <- fixtures.values) {
        val corners = extractCorners(pg).flatten
        assert { corners.map(cn => cn.toListCorner.head == cn).toSet == Set(true) }
      }
    }
  }

  "CornerLine" should {
    "Always say that it is Concave" in {
      for (i <- 0 until 20) {
        val corner = createCornerLine
        assert(corner.isConcave)
      }
    }
  }

  "Chords" should {
    "Always say that it is Concave" in {
      for (i <- 0 until 20) {
        val chord: Chord = createChord

        assert(chord.isConcave)
      }
    }

    "swap method should be an involution" in {
      for (i <- 0 until 20) {
        val chord: Chord = createChord

        assert(chord.swap.swap == chord)
      }
    }
  }
}