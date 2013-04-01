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

import sim.engine.SimState

class OffensiveAntWorker(override val tribe: Tribe) extends AntWorker(tribe) {

  override def receiveHit(opponent: AntWorker) {
    super.receiveHit(opponent)
    if (this.hitpoints == 0) return // Ant dead: no more actions
  }

  override def actMilitarily(state: SimState) {}

  def step(state: SimState) {
    // TODO: Add more behaviour
    actEconomically(state)
  }
}
