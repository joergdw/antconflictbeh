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
package sim.app.antDefenseAIs.model

import sim.engine.SimState

private[antDefenseAIs] object AntQueen {
  val startRessources = 10 /** Amount of resources a tribe starts with. Should be >= `productionCost` */
  val productionTime: Int = 10 /** time to produce an ant*/
  val productionCost: Int = 10 /** costs to produce an ant */
  val maximumAge: Int = Integer.MAX_VALUE /** Maximum age of a queen (in steps) */
  /**
   * Creates an NormalAntWorker
   *
   * @param tribeID Tribe the ant belongs to
   * @param world World the ant lives on
   * @return NormalAntWorker
   */
  private def apply(tribeID: Int, world: World, antGen: AntGenerator) = new AntQueen(tribeID, world, antGen)
}


import AntQueen._

/**
 * Queen of a colony
 *
 * @param tribeID ID of the tribe the ant is member of
 * @param world World the ant lives on
 * @param antGen Constructor of the ant type the queen should use for new ants
 */
private final class AntQueen(
  override val tribeID: Int,
  override val world: World,
  private val antGen: AntGenerator) extends Ant(tribeID, world) {

  override def maximumAge(): Int = AntQueen.maximumAge

  private var _deposit: Int = startRessources /** Resources the queen owns */

  /**
   * Returns the amount of resources in the queens deposit
   *
   * @return Amount of resources in the queens deposit
   */
  def deposit: Int = _deposit

  /*
  0 means: no ant being produced
  every other value between 1 and productionTime is the number of
  leaving time units until the production is completed
   */
  private var productionState: Int = 0

  /**
   * Used to give the queen resources
   *
   * @param amount amount of resources the queen receives
   */
  def receiveRes(amount: Int) {
    assert(amount >= 0)

    _deposit += amount
  }

  /**
   * Queen places all owned resources her current position
   */
  def dropDeposit() {
    val res = world.resOn(currentPos) + deposit
    world.setResOn(currentPos, res)
    _deposit = 0
  }

  /**
   * Queen tries to create new ant.
   *
   * Success iff enough resources available and maximum population not reached
   */
  override def step(state: SimState) {
    assert(0 <= productionState && productionState <= productionTime)

    val tmp = _deposit - productionCost

    if (tmp >= 0 && productionState == 0) { // enough resources and no other construction in progress?
      _deposit = tmp
      productionState += 1
    }

    else if (productionState >= productionTime - 1) { // production completed?
      val ant: Ant = antGen(this)

      try {
        world.placeNewAnt(ant)
        productionState = 0
      } catch {
        case e: IllegalStateException => print(e.getMessage + " by Tribe: " + tribeID)
        case e: Exception => print("Unforeseen Exception: " + e.getMessage)
      }
    }

    else if (productionState > 0 && productionState < productionTime - 1) // production started and not ready?
      productionState += 1  // advance in construction
  }
}