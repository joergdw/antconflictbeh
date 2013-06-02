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


final class Setup_1vs1(var sd: Long) extends Experiment(sd) {

  val (width, height) = (49, 49)
  val lasiusNigerNormal = new LasiusNigerGenerator(new LasiusBehaviourConf())
  val artificialStandardGenerator = new ArtificialAntGenerator(new ArtificialAntBehaviourConf())
  private val tribes: Array[AntGenerator] = Array(lasiusNigerNormal, artificialStandardGenerator)
  override val numberOfTribes = tribes.length

  val resDistrib: Array[Array[Int]] = Array.ofDim(width, height)

  /* Construction of the resource distribution.
   * A map-pattern consisting of some to the two queens symmetric resource-spots
   *
   * One line of resources goes from the one corner to the other.
   */
  {
    for (i <- 0 until height; j <- 0 until width if (i == j) && (i % 16 == 0)) {
      brushSoft(resDistrib, 5, 5, 10, (i, j))
    }

    brushSoft(resDistrib, 6, 5, 20, (39, 9), (9, 39))
  }


  val resourceMap: IntGrid2D = intArray2IntGrid(resDistrib)

  val world: World = new World(experiment = this, height = height, width = width,
    startPositions = Array((0, 0), (width - 1, height - 1)), resources = resourceMap,
    tribeTypes = tribes)


  /**
   * True if only one tribe left and experiment didn't just start
   *
   * @return True if only one tribe left and experiment didn't just start
   */
  override def experimentShouldBeStopped(): Boolean = {
    if (schedule.getSteps >= 1000) return true // TODO: Debug-Code
    if (schedule.getSteps >= 100) { // not stop before at least 100 steps are done
      val populations = world.populationStat()

      var livingTribes: Int = 0
      for (pop <- populations.values)
        if (pop > 1) // check if only queen is left
          livingTribes += 1

      livingTribes <= 1
    }
    else
      false
  }
}
