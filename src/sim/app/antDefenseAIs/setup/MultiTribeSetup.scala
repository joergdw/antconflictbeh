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
  override val tribeTypes: List[AntGenerator] = List(ln_normal_std, ln_normal_std, ln_normal_std,
    ln_normal_std, ln_normal_std, ln_normal_std, ln_normal_std, ln_normal_std, ln_normal_std))
  extends Experiment(sd, tribeTypes) {

  if (tribeTypes.length != 9)
    throw new IllegalArgumentException("MultiTribeSetup only defined for exactly 9 colonies")

  //--------------------------------- Compatibility constructors ----------------------------------
  def this(s: Long) = this(sd = s)


  //--------------------------------- World settings -----------------------------------------------

  val (width, height) = (90, 90)
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
    tribeTypes = tribeTypes)

  override def stopCriteriaFulfilled(): Boolean = schedule.getSteps >= 2000
}
