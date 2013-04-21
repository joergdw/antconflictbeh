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

private[antDefenseAIs] object AntWorker {
  val maximumAge: Int = 5000 /** Maximum age of a worker (in steps) */ // TODO: set to 1000 (imortant for debugging)

  val backpack: Int = 1 /** Amount of resources which can be transported by an individual */
  val notBored: Int = 100 /** Value of boredom, 100 if an ant is not bored at all */

  var gamma: Double = 0.98d /** Learning parameter according the one used paper */
  var explorationRate: Double = 0.4d
}


import StrictMath.{min, max}
import sim.engine.SimState

import AntWorker._

/**
 * What have all ant workers have in common
 *
 * @param tribeID Tribe the ant belongs to
 * @param world World the ant lives on
 */
private[antDefenseAIs] abstract class AntWorker(
  override val tribeID: Int,
  override val world: World) extends Ant(tribeID, world) {

  ///////////////////// sim.app.antDefenseAIs.common variables and constants /////////////////////////////////////

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
    val isBored: Boolean = boredom == 0

    if (backpack_full || isBored) {
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
  def actMilitarily()


  /** Follow home way.
    *
    * The next field is the neighbour-field with the best home-pheromones.
    * Neighbour fields without foreign-colony ants take precedence (to avoid enemy-contact).
    */
  final def followHomeWay() {
    val list: List[(Int, Int)] = nearPos(1).sortBy(homePheroOn).reverse
    val noEnemyList = list filterNot enemySensedOn
    val nextPos = if (noEnemyList.isEmpty) list.head else noEnemyList.head

    moveTo(nextPos)
    adaptResPhero()
  }

  /** Care for food.
    *
    * The next field is most probable the neighbour-field with the best resource-pheromones.
    * With a certain probability (in function of the world.explorationRate) it is any of the
    * neighbour fields.
    */
  final def careForFood() {
    val list: List[(Int, Int)] = nearPos(1).sortBy(resPheroOn).reverse
    val nextPos: (Int, Int) = if (world.random.nextDouble() <= (1.0d - explorationRate))
                                list.head
                              else
                                list.apply(world.random.nextInt(list.size))

    moveTo(nextPos)
    adaptHomePhero()
    adaptResPhero()
    mineRes()
  }

  /**
   * Adapts the home-pheromones of the current field.
   */
  final def adaptHomePhero() {
    val bestNeighbour: (Int, Int) = nearPos(1).sortBy(homePheroOn).reverse.head
    val adaptedValue = if (currentPos == myQueen.currentPos)
                          1.0d
                       else
                          gamma * homePheroOn(bestNeighbour)

    // To avoid pheromone value > 1 and worse value than before
    setHomePheroOn(currentPos, min(1, max(homePheroOn(currentPos), adaptedValue)))
  }

  /**
   * Adapts the ressource-pheromones of the current field.
   */
  final def adaptResPhero() {
    val bestNeighbour: (Int, Int) = nearPos(1).sortBy(resPheroOn).reverse.head
    val adaptedValue = (world.resOn(currentPos) + gamma * resPheroOn(bestNeighbour) / world.maxResAmount)

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
    if (world.random.nextDouble() < mobility) // If ant can
      hitpoints = hitpoints - attack
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
}
