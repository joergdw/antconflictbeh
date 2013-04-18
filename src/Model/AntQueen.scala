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

import java.lang.reflect.Constructor

import sim.engine.SimState

/**
 * Queen of a colony
 *
 * @param tribeID ID of the tribe the ant is member of
 * @param world World the ant lives on
 * @param antGen Constructor of the ant type the queen should use for new ants
 */
final class AntQueen(override val tribeID: Int,
                     override val world: World,
                     private val antGen: AntGenerator) extends Ant(tribeID, world) {

  private var deposit: Int = world.sim.startRessources /** Resources the queen owns */

  def productionTime: Int = world.sim.productionTime
  def productionCost: Int = world.sim.productionCost

  /**
   * Returns the amount of resources in the queens deposit
   *
   * @return Amount of resources in the queens deposit
   */
  def hasDeposit: Int = deposit

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

    deposit += amount
  }

  /**
   * Queen tries to create new ant.
   *
   * Success iff enough resources available and maximum population not reached
   */
  override def step(state: SimState) {
    assert(0 <= productionState && productionState <= productionTime)

    val tmp = deposit - productionCost
    val curPop = world.populationStat(tribeID) // current population of that tribe

    if (tmp >= 0 && productionState == 0) { // enough resources and no other construction in progress?
      deposit = tmp
      productionState += 1
    }

    else if (productionState >= productionTime - 1   // production completed?
             && curPop < world.sim.maxPopulation) {
      productionState = 0

      val ant: Ant = antGen(this)
      world.placeNewAnt(ant)
    }

    else if (productionState > 0 && productionState < productionTime - 1) // production started and not ready?
      productionState += 1  // advance in construction
  }
}

object AntQueen {

  /**
   * Creates an NormalAntWorker
   *
   * @param tribeID Tribe the ant belongs to
   * @param world World the ant lives on
   * @return NormalAntWorker
   */
  def apply(tribeID: Int, world: World, antGen: AntGenerator) = new AntQueen(tribeID, world, antGen)
}