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

import sim.field.grid.IntGrid2D

import sim.app.antDefenseAIs.model._
import sim.app.antDefenseAIs._


class SingleMatchSetup(
  var sd: Long,
  val proband_1: AntGenerator = ln_normal_std,
  val probant_2: AntGenerator = ln_rpb_std)
  extends Experiment(sd) {

  // Compatibility constructor
  def this(s: Long) = this(sd = s)

  val (width, height) = (33, 33)
  private val tribes: Array[AntGenerator] = Array(proband_1, probant_2)
  override val numberOfTribes = tribes.length

  val resDistrib: Array[Array[Int]] = Array.ofDim(width, height)

  // Construction of the resource distribution
  {
    for (column <- 0 until height; row <- 0 until width if column % 16 == 0 && row % 16 == 0) {
      brushSoft(a = resDistrib, width = 5, min_strength =  5, max_strength = 10, poss = List((column, row)))
    }
  }


  val resourceMap: IntGrid2D = intArray2IntGrid(resDistrib)

  val world: World = new World(experiment = this, height = height, width = width,
    startPositions = Array((0, height / 2), (width - 1, height / 2)), resources = resourceMap,
    tribeTypes = tribes)


  override def stopCriteriaFulfilled(): Boolean = {
    val pop_probant_1: Double =  world.colonyInfos(0).population().toDouble
    val pop_probant_2: Double = world.colonyInfos(1).population().toDouble
    val numerousRel: Double = pop_probant_1 / pop_probant_2

    schedule.getSteps >= 1500 ||
      schedule.getSteps >= 400 &&
        (numerousRel < 0.5d || numerousRel > 2.0d)
  }

}
