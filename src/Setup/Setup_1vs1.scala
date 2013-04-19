/*
 * Copyright © 2012 - 2013 by Jörg D. Weisbarth <joerg.bretten@web.de>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License 3 as published by
 * the Free Software Foundation;
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY.
 *
 * See the License.txt file for more details.
 */
package Setup

import sim.field.grid.IntGrid2D

import Model.{World, Simulation, NormalAntWorker, OffensiveAntWorker}
import Common.Common.intArray2IntGrid


final class Setup_1vs1(var sd: Long) extends Simulation(sd) {

  val height = 60
  val width = 60

  val resDistrib: Array[Array[Int]] = Array.ofDim(height, width)
  // TODO: Karte aufbauen

  val resourceMap: IntGrid2D = intArray2IntGrid(resDistrib)

  val world: World = new World(sim = this, height = height, width = width,
    startPositions = Array((0, 0), (59, 59)), resourceMap,
    tribeTypes = Array(NormalAntWorker, OffensiveAntWorker))
}
