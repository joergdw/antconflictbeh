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
package AntDefenseAIs.Setup

import sim.field.grid.IntGrid2D

import AntDefenseAIs.Model._
import AntDefenseAIs.Common.Common.intArray2IntGrid


final class Setup_1vs1(var sd: Long) extends Simulation(sd) {

  val height = 60
  val width = 60
  private val tribes: Array[AntGenerator] = Array(NormalAntWorker, OffensiveAntWorker)
  override val numberOfTribes = tribes.length

  val resDistrib: Array[Array[Int]] = Array.ofDim(height, width)

  /* Construction of the resource distribution.
   * A map-pattern consisting of some to the two queens symmetric resource-spots
   *
   * One line of resources goes from the one corner to the other.
   */
  for (i <- 0 until height; j <- 0 until width if i == j) {
    val maxResAmount: Int = 100 // Maximum number of resources on a field

    resDistrib(i)(j) = maxResAmount
  }

  val resourceMap: IntGrid2D = intArray2IntGrid(resDistrib)

  val world: World = new World(sim = this, height = height, width = width,
    startPositions = Array((0, 0), (59, 59)), resourceMap,
    tribeTypes = tribes)
}
