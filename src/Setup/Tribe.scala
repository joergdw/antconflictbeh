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

import sim.field.grid.{IntGrid2D, DoubleGrid2D}
import sim.engine.{SimState, Steppable}
import sim.util.Int2D
import Tribe.nextTribeID

/** Shared information and data between all the members of a tribe
 *
 * @param tribeID Identifier for the tribe the ant belongs to
 * @param simulation Simulation the tribe participates
 */
final class Tribe private (val tribeID: Int, val simulation: Simulation) extends Steppable {

  /** Pheromone-map of the tribe for going home */
  val homePheros: IntGrid2D = new IntGrid2D(simulation.ants.getHeight,
    simulation.ants.getWidth, Int.MaxValue)

  /** Pheromone-map of the tribe for searching food */
  val resPheros: DoubleGrid2D = new DoubleGrid2D(simulation.ants.getHeight,
    simulation.ants.getWidth, 0.0d)

  /** Pheromone-map of the tribe for war-communication */
  val warPheros: DoubleGrid2D = new DoubleGrid2D(simulation.ants.getHeight,
    simulation.ants.getWidth, 0.0d)

  var queen: AntQueen = null /** Queen of that tribe */

  def this(simulation: Simulation, location: Int2D) = {
    this(nextTribeID(), simulation)

    homePheros.set(location.getX, location.getY, 0) // Queen position is 0
    queen = new AntQueen(this)
    simulation.ants.setObjectLocation(queen, location)
    simulation.schedule.scheduleRepeating(queen)
    simulation.schedule.scheduleRepeating(this, 10) // all 10 turns
  }


  /**
   * Adapts the pheromone maps in function of the time (diffusion, evaporation)
   *
   * @param state Current SimState
   */
  def step(state: SimState) {
    // Evaporation
    for (i <- 0 until simulation.height; j <- 0 until simulation.width) {
      // resPhero.set(i, j, evapore(resPhero.get(i, j))) // Evaporation stopped, because not necessary
      // warPhero.set(i,j, evapore(warPhero.get(i, j)))
    }
  }

  /**
   * Simulates the evaporation
   *
   * @param p Pheromone intensity
   * @return New pheromone intensity
   */
  def evapore(p: Double): Double = if (p < simulation.pheroThreshould) 0
                                   else p - p * (1 - p)
}


object Tribe {
  private var IDOfLastTribe: Int = -1 /** ID of the last generated tribe */  // -1 so that 0 is the ID of the first tribe

  def nextTribeID(): Int = {
    IDOfLastTribe += 1
    IDOfLastTribe
  }

  def apply(simulation: Simulation, location: Int2D): Tribe = new Tribe(simulation, location)
}
