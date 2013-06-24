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

import sim.app.antDefenseAIs.common.Common.intArray2IntGrid
import sim.app.antDefenseAIs.setup.MapCreationHelpers._
import sim.app.antDefenseAIs.model._


class Setup_1vs1(var sd: Long) extends Experiment(sd) {

  val (width, height) = (33, 33)
  val lasiusNigerNormal = new LN_Generator(new LasiusBehaviourConf())
  val lasiusNigerAggressive = new LN_Generator(
    new LasiusBehaviourConf(
      maxAggressiveness = 10, maxAggressivenessProb = 0.9, minAggressivenessProb = 0.5))
  val artificialNormal = new OAD_Generator(new OAD_BehaviourConf())
  private val tribes: Array[AntGenerator] = Array(lasiusNigerNormal, artificialNormal)
  override val numberOfTribes = tribes.length

  val resDistrib: Array[Array[Int]] = Array.ofDim(width, height)

  // Construction of the resource distribution
  {
    for (column <- 0 until height; row <- 0 until width if (column % 16 == 0) && (row % 16 == 0)) {
      brushSoft(a = resDistrib, width = 5, min_strength =  5, max_strength = 10, poss = (column, row))
    }
  }


  val resourceMap: IntGrid2D = intArray2IntGrid(resDistrib)

  val world: World = new World(experiment = this, height = height, width = width,
    startPositions = Array((0, height / 2), (width - 1, height / 2)), resources = resourceMap,
    tribeTypes = tribes)


  override def stopCriteriaFulfilled(): Boolean = schedule.getSteps >= 1500
}
