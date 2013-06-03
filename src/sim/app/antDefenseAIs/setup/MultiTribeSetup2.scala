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

class MultiTribeSetup2(var sd: Long) extends Experiment(sd) {

  val (width, height) = (90, 90)
  val lasiusNigerNormal = new LasiusNigerGenerator(new LasiusBehaviourConf())
  val artificialStandardGenerator = new ArtificialAntGenerator(new ArtificialAntBehaviourConf())
  private val tribes: Array[AntGenerator] = Array(artificialStandardGenerator, lasiusNigerNormal, lasiusNigerNormal,
    lasiusNigerNormal, lasiusNigerNormal, lasiusNigerNormal, lasiusNigerNormal, lasiusNigerNormal, lasiusNigerNormal)
  override val numberOfTribes = tribes.length

  val resDistrib: Array[Array[Int]] = Array.ofDim(height, width)

  /* Construction of the resource distribution.
   * A map-pattern consisting of horizontal lines of resource-spots
   */
  {
    for (column <- 0 until height; row <- 0 until width if ((column - 5) % 16 == 0) && ((row + 12) % 16 == 0)) {
      brushSoft(resDistrib, 5, 5, 10, (column, row))
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

  override def stopCriteriaFulfilled(): Boolean = schedule.getSteps >= 5000
}
