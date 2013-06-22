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
package sim.app.antDefenseAIs.model

import sim.engine.SimState

private[antDefenseAIs] object AntQueen {
  val startRessources = 15 /** Amount of resources a tribe starts with. Should be >= `productionCost` */
  val productionTime: Int = 10 /** time to produce an ant*/
  val productionCost: Int = 15 /** costs to produce an ant */
  val maximumAge: Int = Integer.MAX_VALUE /** Maximum age of a queen (in steps) */
  /**
   * Creates an NormalAntWorker
   *
   * @param tribeID Tribe the ant belongs to
   * @param world World the ant lives on
   * @return NormalAntWorker
   */
  private def apply(tribeID: Int, world: World, conf: BehaviourConf, antGen: AntGenerator) =
    new AntQueen(tribeID, world, conf, antGen)
}


import AntQueen._

/**
 * Queen of a colony
 *
 * @param antGen Constructor of the ant type the queen should use for new ants
 */
private[antDefenseAIs] final class AntQueen(
  override val tribeID: Int,
  override val world: World,
  override val behaviourConf: BehaviourConf,
  private val antGen: AntGenerator) extends Ant(tribeID, world, behaviourConf) {

  override def maximumAge(): Int = AntQueen.maximumAge
  override val pheroSystem = new StandardPheroSystem(this)

  _inBackpack = startRessources /** Resources the queen owns */

  /*
  0 means: no ant being produced
  every other value between 1 and productionTime is the number of
  leaving time units until the production is completed
   */
  private var productionState: Int = 0

  /**
   * Used to give the queen resources
   *
   * @param amount intensity of resources the queen receives
   */
  def receiveRes(amount: Int) {
    assert(amount >= 0)

    _inBackpack += amount
  }

  /**
   * Queen places all owned resources her current position
   */
  def dropDeposit() {
    val res = world.resOn(currentPos) + _inBackpack
    world.setResOn(currentPos, res)
    _inBackpack = 0
  }

  /**
   * Queen tries to create new ant.
   *
   * Success iff enough resources available and maximum population not reached
   */
  override def step(state: SimState) {
    assert(0 <= productionState && productionState <= productionTime)

    val tmp = _inBackpack - productionCost

    if (tmp >= 0 && productionState == 0) { // enough resources and no other construction in progress?
      _inBackpack = tmp
      productionState += 1
    }

    else if (productionState >= productionTime - 1) { // production completed?
      val ant: Ant = antGen(this)

      try {
        world.placeNewAnt(ant)
        productionState = 0
      } catch {
        case e: IllegalStateException => println(e.getMessage)
      }
    }

    else if (productionState > 0 && productionState < productionTime - 1) // production started and not ready?
      productionState += 1  // advance in construction
  }
}