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
import StrictMath.{min, max}

/**
 * What have all ant workers have in common
 *
 * @param tribeID Tribe the ant belongs to
 * @param world World the ant lives on
 */
abstract class AntWorker (override val tribeID: Int,
                          override val world: World) extends Ant(tribeID, world) {

  ///////////////////// Common variables and constants /////////////////////////////////////

  val backpack: Int = 1 /** amount of resources which can be transported by an individual */
  val notBored: Int = 100 /** value of boredom, 100 if an ant is not bored at all */

  protected var transporting: Int = 0 /** Amount of resources transported by this ant */
  protected var boredom: Int = notBored /** 0 if an ant is „bored“ of searching abortively food and wants to go home */


  ///////////////////// Behaviour description ////////////////////////////////////////

  override def step(state: SimState)

  /** Actions for ants serving the economy of its tribe.
    *
    * If the backpack is full, or the ant is bored that is the ant has searched too long resources
    * without success, the ant follows the way home to its queen and give all resources in the backpack
    * to her. (After that ant is not bored at all.)
    *
    * In any other case the ant cares for food.
    */
  final def actEconomically(state: SimState) {

    val backpack_full: Boolean = transporting >= backpack
    val is_bored: Boolean = boredom == 0

    if (backpack_full || is_bored) {
      if (currentPos == myQueen.currentPos) { // queen is under the ant
        dropResources()
        boredom = notBored
      }
      else
        followHomeWay()

    }
    else
      careForFood()
  }

  /** Actions when ant want to fight or to flee – dependent of the ant-type */
  def actMilitarily(state: SimState)


  /** Follow home way.
    *
    * The next field is the neighbour-field with the best home-pheromones.
    * Neighbour fields without foreign-colony ants take precedence (to avoid enemy-contact).
    */
  final def followHomeWay() {
    val list: List[(Int, Int)] = nearPos(1) sortBy (homePheroOn)
    val noEnemyList = list filterNot enemySensedOn
    val nextPos = if (noEnemyList.isEmpty) list.head else noEnemyList.head

    moveTo(nextPos)
    adaptResPhero()
  }

  /** Care for food.
    *
    * The next field is ost probable the neighbour-field with the best resource-pheromones.
    * With a certain probability (in function of the sim.explorationRate) it is any of the
    * neighbour fields.
    */
  final def careForFood() {
    val list: List[(Int, Int)] = (nearPos(1) sortBy (resPheroOn)).reverse
    val nextPos: (Int, Int) = if (world.sim.random.nextDouble() <= (1.0d - world.sim.explorationRate))
                                list.head
                              else
                                list.apply(world.sim.random.nextInt(list.size))

    moveTo(nextPos)
    adaptHomePhero()
    adaptResPhero()
    mineRes()
  }

  /**
   * Adapts the home-pheromones of the current field.
   */
  final def adaptHomePhero() {
    val pos = currentPos
    val currentValue: Int = homePheroOn(pos)

    val sortedNeighbours: List[(Int, Int)] =  nearPos(1) sortBy (homePheroOn)
    val bestNeighbour: Int = homePheroOn(sortedNeighbours.head)

    // To avoid arithmetic overflow and worse distance
    setHomePheroOn(pos, min(currentValue, max(bestNeighbour, bestNeighbour + 1)))
  }

  /**
   * Adapts the ressource-pheromones of the current field.
   */
  final def adaptResPhero() {
    val bestNeighbour: (Int, Int) = (nearPos(1) sortBy (resPheroOn) reverse).head
    val adaptedValue = (world.resOn(currentPos) + world.sim.gamma * resPheroOn(bestNeighbour) / world.sim.maxResAmount)

    setResPheroOn(currentPos, min(1, adaptedValue))
  }


  //////////////////// Basic operations of ants //////////////////////////////////////

  /**
   * Drops the resources on the current place. If the queen is there, she
   * receives them.
   */
  final def dropResources() {
    val pos = currentPos
    if (pos == myQueen.currentPos)
      myQueen.receiveRes(transporting)
    else {
      val res = world.resOn(pos) + transporting
      world.setResOn(pos, res)
    }

    transporting = 0
  }

  /**
   * Mines, if possible, resources. Boredom increased if no resources.
   * No boredom if try successful.
   */
  final def mineRes() {
    val pos = currentPos
    val spaceLeft: Boolean = backpack > transporting // space left in bag?
    if (spaceLeft && world.resOn(pos) > 0) {
      world.setResOn(pos, world.resOn(pos) - 1)
      transporting += 1
      boredom = notBored
    }
    else
      boredom -= 1
  }

  /**
   * Adaptions after receiving a hit
   */
  def receiveHit(opponent: AntWorker) {
    hitpoints = hitpoints - opponent.attack
  }

  /**
   * Hit an opponent
   *
   * @param opponent Opponent receiving a hit
   */
  def hit(opponent: AntWorker) {
    opponent receiveHit(this)
  }


  //////////////////////// Information revealing functions for other classes ////////////

  /**
   * Amount of resources transported by this ant
   *
   * @return Amount of resources transported by this ant
   */
  final def inBackpack: Int = transporting

  /**
   * Returns true if the ant is dead
   *
   * @return True iff ant is dead
   */
  final def isDead: Boolean = hitpoints == 0
}
