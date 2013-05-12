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
  val maximumAge: Int = 5000 /** Maximum age of a ant (in steps) */
  val backpackSize: Int = 5 /** Amount of resources which can be transported by an individual */
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
  override val world: World,
  val behaviourConf: BehaviourConf) extends Ant(tribeID, world) {
  import behaviourConf._

  ///////////////////// Common variables and constants /////////////////////////////////////

  protected var transporting: Int = 0 /** Amount of resources transported by this ant */
  override final def maximumAge(): Int = AntWorker.maximumAge


  ///////////////////// Behaviour description ////////////////////////////////////////

  /**
   * Chooses a neighbourfield to go to.
   *
   * Works in the following way: With a probability of (1 - `explorationRate`) the position with
   * the best evaluation is chosen. In the other case there will be chosen a random position
   * from the rest of the fields.
   *
   * @param pheroOn Pheromone to observe for determining next position.
   * @return
   */
  final def chooseDirectionByPheromone(pheroOn: ((Int, Int)) => Double): world.Direction.Value = {
    def valueDirection = valueDirectionWithPhero(pheroOn) _

    // Add to every direction its value
    val directionsValued: List[(world.Direction.Value, Double)] =
      validDirections.map(x => (x, valueDirection(x)))

    val valDirsSorted = directionsValued.sortBy(x => x._2).reverse

    if (world.random.nextDouble() <= (1.0d - explorationRate))
      valDirsSorted.head._1
    else
      valDirsSorted.apply( 1 + world.random.nextInt(valDirsSorted.size - 1))._1
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

    alpha * dirValueByPhero(dir) + (1 - alpha) * dirValueByDir(dir)
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
   * Mines, if possible, resources.
   */
  def mineRes() {
    val pos = currentPos
    val spaceLeft: Boolean = backpackSize > transporting // space left in bag?
    if (spaceLeft && world.resOn(pos) > 0) {
      world.setResOn(pos, world.resOn(pos) - 1)
      transporting += 1
    }
  }

  /**
   * Adapts the home-pheromones of the current field.
   */
  def adaptHomePhero() {
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
  def adaptResPhero() {
    val bestNeighbour: (Int, Int) = nearPos(1).sortBy(resPheroOn).reverse.head
    val adaptedValue = (world.resOn(currentPos) + gamma * resPheroOn(bestNeighbour) / world.maxResAmount)

    setResPheroOn(currentPos, min(1, adaptedValue))
  }

  /**
   * Adds to each direction the amount of pheromones lying there
   *
   * @return List of directions zipped with corresponding pheromone value
   */
  def directionsWithHomePhero() = directionsWithPhero(homePheroOn)

  /**
   * Adds to each direction the amount of pheromones lying there
   *
   * @return List of directions zipped with corresponding pheromone value
   */
  def directionsWithResPhero() = directionsWithPhero(resPheroOn)

  /**
   * Adds to each direction the amount of pheromones lying there
   *
   * @return List of directions zipped with corresponding pheromone value
   */
  def directionsWithWarPhero() = directionsWithPhero(warPheroOn)


  //////////////////////// Helpers /////////////////////////////////

  /**
   * Adds to each direction the amount of pheromones lying there
   *
   * @param pheroOn Function revealing pheromone on a certain position
   * @return List of directions zipped with corresponding pheromone value
   */
  private def directionsWithPhero(pheroOn: ((Int, Int)) => Double): List[(world.Direction.Value, Double)] = {
    def pheroInDir(dir: world.Direction.Value): (world.Direction.Value, Double) = {
      val position = world.Direction.inDirection(currentPos, dir)
      (dir, pheroOn(position))
    }

    world.Direction.values.toList.map(pheroInDir)
  }


  //////////////////////// Information revealing functions for other classes ////////////

  /**
   * Amount of resources transported by this ant
   *
   * @return Amount of resources transported by this ant
   */
  final def inBackpack: Int = transporting
}
