/*
 * Copyright © 2013 by Jörg D. Weisbarth <joerg.bretten@web.de>
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


final class Setup_1vs1(var sd: Long) extends Simulation(sd) {

  val height = 57
  val width = 57
  val lasiusNigerStandardGenerator = new LasiusNigerGenerator(new LasiusBehaviourConf())
  val artificialStandardGenerator = new ArtificialAntGenerator(new ArtificialAntBehaviourConf())
  private val tribes: Array[AntGenerator] = Array(artificialStandardGenerator, artificialStandardGenerator) // TODO: Adapt tribe
  override val numberOfTribes = tribes.length

  val resDistrib: Array[Array[Int]] = Array.ofDim(height, width)

  /* Construction of the resource distribution.
   * A map-pattern consisting of some to the two queens symmetric resource-spots
   *
   * One line of resources goes from the one corner to the other.
   */
  {
    for (i <- 0 until height; j <- 0 until width if (i == j) && (i % 8 == 0)) {
      brushSoft(resDistrib, 5, 5, 10, (i, j))
    }

    brushSoft(resDistrib, 5, 5, 20, (43, 13), (13, 43))
  }


  val resourceMap: IntGrid2D = intArray2IntGrid(resDistrib)

  val world: World = new World(sim = this, height = height, width = width,
    startPositions = Array((0, 0), (height - 1, width - 1)), resources = resourceMap,
    tribeTypes = tribes)
}
