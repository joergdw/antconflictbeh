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
package Model

import java.lang.reflect.Constructor // TODO: Überprüfen, ob das der richtige Konstruktor ist

import sim.engine.SimState
import sim.field.grid.IntGrid2D

/**
 * Contains some preferences for the simulation
 *
 * @param s Seed for random data generator
 */
abstract class Simulation(var s: Long) extends SimState(s) {
  // TODO: Because of its high logical and theoretical coherence with the World-Class it is better to merge them.

  val nTribes: Int = 2 /** Number of tribes set on the map */

  val world: World

  val gamma: Double = 0.9d /** Learning parameter according the one used paper */
  val explorationRate: Double = 0.2d
  val maxResAmount: Int = 10 /** Maximum number of resources on a field */
  val pheroThreshould: Double = 0.0000000001d /** Next phero-value: zero */

  val maxPopulation: Int = Int.MaxValue /** Maximum tribe population */
  val startRessources: Int = 20 /** amount of res a tribe starts with */
  val productionTime: Int = 10 /** time to produce an ant*/
  val productionCost: Int = 1 /** costs to produce an ant */

  override def start() {
    super.start()
    world.start()
  }
}