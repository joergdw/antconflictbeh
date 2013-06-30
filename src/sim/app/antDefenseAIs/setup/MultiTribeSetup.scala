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

class MultiTribeSetup(
  var sd: Long,
  val proband: AntGenerator = ln_normal_std,
  val participating: Array[AntGenerator] = Array())
  extends Experiment(sd) {

  // Compatibility constructor
  def this(s: Long) = this(sd = s)

  val (width, height) = (90, 90)
  private val tribes: Array[AntGenerator] = {
    val tribes = Array(proband, ln_normal_std, ln_normal_std,
      ln_normal_std, ln_normal_std, ln_normal_std, ln_normal_std, ln_normal_std, ln_normal_std)

    if (participating.length > tribes.length - 1)
      throw new IllegalArgumentException("Too many participating colonies defined!")

    for (i <- 0 until participating.length) {
      tribes(i + 1) = participating(i)
    }

    tribes
  }
  override val numberOfTribes = tribes.length

  val resDistrib: Array[Array[Int]] = Array.ofDim(height, width)

  /* Construction of the resource distribution.
   * A map-pattern consisting of horizontal lines of resource-spots
   */
  {
    for (column <- 0 until height; row <- 0 until width if (column - 5) % 16 == 0 && (row + 12) % 16 == 0) {
      brushSoft(a = resDistrib, width = 5, min_strength = 5, max_strength = 10, poss = List((column, row)))
    }
  }


  val resourceMap: IntGrid2D = intArray2IntGrid(resDistrib)
  val startPositions = Array(
    (width/2, height/2),
    (0, 0),
    (width/2, 0),
    (width - 1, 0),
    (0, height/2),
    (width - 1, height/2),
    (0, height - 1),
    (width/2, height - 1),
    (width - 1, height - 1)
  )

  val world: World = new World(experiment = this, height = height, width = width,
    startPositions = startPositions, resources = resourceMap,
    tribeTypes = tribes)

  override def stopCriteriaFulfilled(): Boolean = schedule.getSteps >= 2000
}
