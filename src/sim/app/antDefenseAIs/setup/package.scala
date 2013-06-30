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

package sim.app.antDefenseAIs

import sim.app.antDefenseAIs.model._
import java.lang.StrictMath._

/**
 * Common ant creators and map design utilities
 */
package object setup {


  //------------------------- Ant creators --------------------------------------------

  // The naming convention is: <ant-family>_<subfamily>_<configuration> abbreviations could be used.

  val ln_normal_std = new LN_Normal_Generator(new LN_Normal_BehaviourConf())

  val ln_rpb_std = new LN_RPB_Generator(new LN_RPB_BehaviourConf())

  val ln_pb_std = new LN_PB_Generator(new LN_PB_BehaviourConf())

  val ln_rpb_wf_std = new LN_RPB_WF_Generator(new LN_RPB_WF_BehaviourConf())


  //-------------------------- Map design utilities ------------------------------------
  /**
   * Sets the value `strength` on all positions within a certain distance (`width`) towards one of the points in `poss`.
   *
   * @param a 2-dim Array to operate on
   * @param width Brush-width
   * @param strength Brush-strength (value to place on)
   * @param poss Positions in the center of the brush
   */
  def brush(a: Array[Array[Int]], width: Int, strength: Int, poss: (Int, Int)*) {
    brushSoft(a = a, width = width, min_strength = strength, max_strength = strength, poss = poss.toList)
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
