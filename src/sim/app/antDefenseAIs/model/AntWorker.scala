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
  val maximumAge: Int = 5000 /** Maximum age of a worker (in steps) */
  val backpack: Int = 5 /** Amount of resources which can be transported by an individual */
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
private[antDefenseAIs] class AntWorker(
  override val tribeID: Int,
  override val world: World,
  protected val behaviour: AntBehaviour) extends Ant(tribeID, world) {

  ///////////////////// Common types, variables and constants /////////////////////////////////////

  protected var transporting: Int = 0 /** Amount of resources transported by this ant */
  override final def maximumAge(): Int = AntWorker.maximumAge

  ///////////////////// Behaviour description ////////////////////////////////////////

  override def step(state: SimState) = behaviour.step(state)


  //////////////////// Basic operations of ants //////////////////////////////////////


  private[mode] def directionsWithHomePhero()

  private[mode] def directionsWithResPhero()

  private[mode] def directionsWithWarPhero()

  private def directionsWithPhero(pheroOn: ((Int, Int)) => Double): List[(world.Direction.Value, Double)] = {
    def pheroInDir(dir: world.Direction.Value): (world.Direction.Value, Double) = {
      val position = world.Direction.inDirection(currentPos, dir)
      (dir, pheroOn(position))
    }

    world.Direction.values.toList.map(pheroInDir)
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

  override protected def receiveHit(opponent: Ant) {
    super.receiveHit(opponent)
    behaviour.reactToHit()
  }

  /**
   * Counts the number of ants within the neighbourhood fulfilling a predicate.
   *
   * @param range Range in which will be searched
   * @param p Predicate
   * @return Number of ants in the neighbourhood fulfilling the predicate p
   */
  def countAntsFullfillingPredicate(range: Int)(p: Ant => Boolean): Int = {
    val ants: List[Ant] = listAntsFullfillingPredicate(range)(p)
    def adder(i: Int, a: Ant): Int = i + (if (p(a)) 1 else 0)
    ants.foldLeft(0: Int)(adder)
  }

  /**
   * List of all ants within the neighbourhood fulfilling a predicate.
   *
   * @param range Range in which will be searched
   * @param p Predicate
   * @return Number of ants in the neighbourhood fulfilling the predicate p
   */
  def listAntsFullfillingPredicate(range: Int)(p: Ant => Boolean): List[Ant] =
    neighbourhood(range).map(world.antsOn).flatten

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

  //////////////////////// Information revealing functions for other classes ////////////

  /**
   * Amount of resources transported by this ant
   *
   * @return Amount of resources transported by this ant
   */
  final def inBackpack: Int = transporting
}
