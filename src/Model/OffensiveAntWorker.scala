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

import sim.engine.SimState

/**
 * Antworker with a more offensive behaviour
 *
 * @param tribeID Tribe the ant belongs to
 * @param world World the ant lives on
 */
class OffensiveAntWorker(override val tribeID: Int,
                         override val world: World) extends AntWorker(tribeID, world) {

  /**
   * Constructs ant with the information of the given ant
   *
   * @param ant Ant giving the information of construction
   * @return Ant of the same colony in the same simulation
   */
  def this(ant: Ant) = this(ant.tribeID, ant.world)

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

object OffensiveAntWorker extends AntGenerator {

  /**
   * Creates an NormalAntWorker
   *
   * @param tribeID Tribe the ant belongs to
   * @param world World the ant lives on
   * @return NormalAntWorker
   */
  def apply(tribeID: Int, world: World) = new NormalAntWorker(tribeID, world)

  def apply(ant: Ant) = new OffensiveAntWorker(ant)
}