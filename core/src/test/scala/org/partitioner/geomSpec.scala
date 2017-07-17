package org.partitioner

import org.partitioner.orthogonal._
import org.scalatest.{Matchers, WordSpec}

import scala.util.Random


class GeometriesSpec extends WordSpec with Matchers with PolygonFixtures {
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
        corners.flatten.map(_.toListCorner.length).toSet should be (Set(1))
      }
    }

    "toListCorner should contain itself" in {
      for (pg <- fixtures.values) {
        val corners = extractCorners(pg).flatten
        corners.map(cn => cn.toListCorner.head == cn).toSet should be (Set(true))
      }
    }
  }

  "CornerLine" should {
    "Always say that it is Concave" in {
      for (i <- 0 until 20) {
        val corner = createCornerLine
        corner.isConcave should be (true)
      }
    }
  }

  "Chords" should {
    "Always say that it is Concave" in {
      for (i <- 0 until 20) {
        val chord: Chord = createChord

        chord.isConcave should be (true)
      }
    }

    "swap method should be an involution" in {
      for (i <- 0 until 20) {
        val chord: Chord = createChord

        chord.swap.swap shouldEqual chord
      }
    }
  }
}