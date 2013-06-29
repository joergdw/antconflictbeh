/*
 * Copyright © 2013 by Jörg D. Weisbarth
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License 3 as published by
 * the Free Software Foundation;
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY.
 *
 * See the License.txt file for more details.
 */

package sim.app.antDefenseAIs.setup

import StrictMath.max

import sim.app.antDefenseAIs._

/**
 * Offers help methods for map creations
 */
private object MapCreationHelpers {

  /**
   * Sets the value `strength` on all positions within a certain distance (`width`) towards one of the points in `poss`.
   *
   * @param a 2-dim Array to operate on
   * @param width Brush-width
   * @param strength Brush-strength (value to place on)
   * @param poss Positions in the center of the brush
   */
  def brush(a: Array[Array[Int]], width: Int, strength: Int, poss: (Int, Int)*) {
    brushSoft(a, width, strength, strength, poss.toList)
  }

  /**
   * Sets an value interpolated between `min_strength` and `max_strength` on all positions within a certain distance
   * (`width`) towards one of the points in `poss`.
   *
   * @param a 2-dim Array to operate on
   * @param width Brush-width
   * @param min_strength Minimum brush-strength  (lowest value to be placed)
   * @param max_strength Maximum brush-strength (highest value to be placed)
   * @param poss Positions in the center of the brush
   */
  def brushSoft(a: Array[Array[Int]], width: Int, min_strength: Int, max_strength: Int, poss: (Int, Int)*) {
    brushSoft(a, width, min_strength, max_strength, poss.toList)
  }

  /**
   * Sets an value interpolated between `min_strength` and `max_strength` on all positions within a certain distance
   * (`width`) towards one of the points in `poss`.
   *
   * @param a 2-dim Array to operate on
   * @param width Brush-width
   * @param min_strength Minimum brush-strength  (lowest value to be placed)
   * @param max_strength Maximum brush-strength (highest value to be placed)
   * @param poss Positions in the center of the brush
   */
  def brushSoft(a: Array[Array[Int]], width: Int, min_strength: Int, max_strength: Int, poss: List[(Int, Int)]) {

    for (pos <- poss) {
      def interpolate(p: (Int, Int)): Int = {
        val alpha = (width - maxDistance(p, pos)) / width
        (1 - alpha) * min_strength + alpha * max_strength
      }

      val neighbourhood = neighbourhoodOf(a.size, a(0).size, pos, width)

      for (field <- neighbourhood) {
        a(field._1)(field._2) = max(a(field._1)(field._2), interpolate(field))
      }
    }
  }
}
