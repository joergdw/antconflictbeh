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
  val maximumAge: Int = 2500 /** Maximum age of a worker (in steps) */

  val backpack: Int = 1 /** Amount of resources which can be transported by an individual */
  val notBored: Int = 100 /** Value of boredom, 100 if an ant is not bored at all */

  // The sum of the following two parameters should be exactly 1
  var alpha: Double = 0.7d /** Influence of pheromon for determin next position. Should be between 0 and 1 */
  var beta: Double = 0.3d /** Influence of old direction for determin next position. Should be between 0 and 1 */

  var explorationRate: Double = 0.4

  var gamma: Double = 0.98d /** Learning parameter according the one used paper */
}


import StrictMath.{min, max, abs}
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

  ///////////////////// Common variables and constants /////////////////////////////////////

  protected var transporting: Int = 0 /** Amount of resources transported by this ant */
  protected var boredom: Int = notBored /** 0 if an ant is „bored“ of searching abortively food and wants to go home */
  override final def maximumAge(): Int = AntWorker.maximumAge

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
  final protected def actEconomically(state: SimState) {

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
  protected def actMilitarily()


  /**
   * Follow home way.
   *
   * The next field is most probable one of the neighbour-fields with the best home-pheromones.
   * With a certain probability (in function of the world.explorationRate) it is one of the other fields.
   */
  final protected def followHomeWay() {
    val direction = chooseDirectionByPheromone(homePheroOn)
    moveTo(direction)
    adaptHomePhero()
    adaptResPhero()
  }

  /**
   * Care for food.
   *
   * The next field is most probable one of the neighbour-fields with the best resource-pheromones.
   * With a certain probability (in function of the world.explorationRate) it is one of the other fields
   */
  final protected def careForFood() {
    val direction = chooseDirectionByPheromone(resPheroOn)
    moveTo(direction)
    adaptHomePhero()
    adaptResPhero()
    mineRes()
  }

  /**
   * Chooses a neighbourfield to go to.
   *
   * Works in the following way: With a probability of (1 - `explorationRate`) one of the positions with
   * the highest pheromone concentration is chosen. One of the "highest" means: the differ only from the
   * highest concentration by an value of `epsilon`. In the other case there will be chosen a random position
   * from the rest of the fields.
   *
   * @param pheroOn Pheromone to observe for determining next position.
   * @return
   */
  final def chooseDirectionByPheromone(pheroOn: ((Int, Int)) => Double): world.Direction.Value = {
    import sim.app.antDefenseAIs.common.Common.epsilon

    def valueDirection = valueDirectionWithPhero(pheroOn) _

    // Add to every direction its value
    val directionsValued: List[(world.Direction.Value, Double)] =
      validDirections.map(x => (x, valueDirection(x)))

    val valDirsSorted = directionsValued.sortBy(x => x._2).reverse

    // Tells when a direction is considered to be equal to the best one
    def valueIsEqual(valDir: (world.Direction.Value, Double)) =
      abs(valDir._2 - valDirsSorted.head._2) < epsilon

    val bestNeighbours = valDirsSorted.filter(valueIsEqual)
    val otherNeighbours = directionsValued.filterNot(valueIsEqual)

    if (bestNeighbours.size > 0 && otherNeighbours.size > 0) {
      if (world.random.nextDouble() <= (1.0d - explorationRate)) {
        bestNeighbours.apply(world.random.nextInt(bestNeighbours.size))._1
      } else {
        otherNeighbours.apply(world.random.nextInt(otherNeighbours.size))._1
      }
    } else if (bestNeighbours.size > 0) {
      bestNeighbours.apply(world.random.nextInt(bestNeighbours.size))._1
    } else {
      otherNeighbours.apply(world.random.nextInt(otherNeighbours.size))._1
    }
  }

  // Calculates an all over all value for a direction
  final def valueDirectionWithPhero(pheroOn: ((Int, Int)) => Double)(dir: world.Direction.Value): Double = {
    // Calculates a normalized value of a direction influenced by the pheromone
    def dirValueByPhero(dir: world.Direction.Value): Double = {
      val bestPheroInNeighbourhood = neighbourhood(1).map(pheroOn).max

      val targetPos = world.Direction.inDirection(currentPos, dir)
      if (bestPheroInNeighbourhood == 0)
        0
      else
        pheroOn(targetPos) / bestPheroInNeighbourhood
    }

    // Calculates a normalized value of a direction influenced by the last direction
    def dirValueByDir(dir: world.Direction.Value): Double =
      world.Direction.directionDistance(lastDirection, dir) / world.Direction.MaxDirDistance

    alpha * dirValueByPhero(dir) + beta * dirValueByDir(dir)
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
