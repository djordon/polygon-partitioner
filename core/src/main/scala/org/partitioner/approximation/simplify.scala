package org.partitioner

import scala.annotation.switch
import com.vividsolutions.jts.simplify.{DouglasPeuckerSimplifier, TopologyPreservingSimplifier}
import com.vividsolutions.jts.geom.{Geometry, Polygon}


/**
 * Returns a simplified version of the input polygon.
 *
 * Under the hood this calls the simplify method in
 * DouglasPeuckerSimplifier or TopologyPreservingSimplifier
 * from JTS. When preserve is false DouglasPeuckerSimplifier is used.
 * This means the output can be split, the output can collapse into
 * lines or disappear entirely, holes can be created or expected holes
 * can disappear, and lines can cross. On the plus side, it is much
 * faster than the TopologyPreservingSimplifier based approached
 * that corresponds to when preserve is true.
 *
 * @param polygon The input polygon.
 * @param tolerance The tolerance to use when simplifying the boundary.
 *                  Must be non negative. Greater values imply a coarser
 *                  (less accurate) output polygon.
 * @param preserve Specifies whether the simplifying algorithm should ensure
 *                 that the topology of the input polygon is preserved.
 *
 * @return Returns a simplified polygon that has been normalized.
 */
object simplify extends Function3[Polygon, Double, Boolean, Polygon] {

  def apply(polygon: Polygon, tolerance: Double, preserve: Boolean = false): Polygon = {

    val simplifier = (preserve: @switch) match {
      case true => TopologyPreservingSimplifier.simplify _
      case false => DouglasPeuckerSimplifier.simplify _
    }

    if (0 <= tolerance && tolerance < Double.PositiveInfinity) {
      val newPolygon: Geometry = simplifier(polygon.norm, tolerance)
      newPolygon.normalize()
      newPolygon.asInstanceOf[Polygon]
    } else
      polygon.norm.asInstanceOf[Polygon]
  }
}
