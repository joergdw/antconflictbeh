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

private[antDefenseAIs] object Ant {
  val maximumHitpoints = 10 /** How many hitpoints an undamaged individual has */
  val antsSensingRange = 2 /** Radius of the area the ant can sense other individuals */
}


import sim.engine.Steppable
import Ant._

/**
 * Common properties and capacities of ants
 */
private[antDefenseAIs] abstract class Ant extends Steppable {
  val tribeID: Int /** Tribe the ant is member of */
  val world: World /** World the ant lives on */

  protected final var _hitpoints: Int = maximumHitpoints /** How much an individual can suffer before dieing */
  protected final var mobility: Float = 0f /** Probability to avoid to be hit */
  protected final var attack: Int = 1 /** Damage an ant does to another */
  protected final var _age: Int = 0 /** Current age of the ant */
  protected final var _inBackpack: Int = 0 /** Amount of resources transported by this ant */

  val alpha: Double /** Influence of pheromone for determine next position. Should be between 0 and 1 */
  val explorationRate: Double /** Probability that another than the best neighbour field will be chosen to move to */


  /** Last went direction */
  protected final var lastDirection: Direction.Value = {  // Initialise with random value
    val valueList = Direction.values.toList
    valueList.apply(world.random.nextInt(Direction.values.size))
  }


  // ----------- Information revealing functions for other classes ----------------------------

  final def inBackpack: Int = _inBackpack
  final def hitpoints: Int = _hitpoints
  def maximumAge: Int
  def age: Int = _age
  final def isKilled: Boolean = _hitpoints <= 0
  final def isOveraged: Boolean = _age >= maximumAge


  // ------------ Basic interaction functions with World ----------------------------

  private[model] final def mature() {_age += 1}

  /**
   * Adaptions after receiving a hit
   */
  protected[model] def receiveHitFrom(opponent: Ant) {
    if (world.random.nextDouble() >= mobility) // If ant can ...
      _hitpoints = _hitpoints - attack
  }

  /**
   * Hit an opponent
   *
   * @param opponent Opponent receiving a hit
   */
  protected[model] def hit(opponent: Ant) {
    world.hit(this)(opponent)
  }

  /**
   * Current position of that ant as (Int, Int)
   *
   * @return Current position of that ant as (Int, Int)
   */
  protected[model] def currentPos: (Int, Int) = world.currentPosOf(this).get

  /**
   * All directions in which exists another field of the map
   *
   * @return All directions in which exists another field of the map
   */
  protected[model] def validDirections: List[Direction.Value] = world.validDirections(this).get

  /**
   * All directions in which exists another field of the map
   *
   * @return All directions in which exists another field of the map
   */
  protected[model] def accsessibleDirections: List[Direction.Value] = world.accessibleDirections(this).get

  /**
   * Returns a reference to the queen of the ant
   *
   * @return Queen of the ant
   */
  protected[model] def myQueen: AntQueen = world.queenOf(this)

  /**
   * Moves ant into the given direction
   *
   * @param direction New position of the ant
   */
  protected[model] def moveTo(direction: Direction.Value) {
    world move(this, direction)
    lastDirection = direction
  }

  // ------------------------ Environment observation functions -------------------------------------------

  /**
   * Counts the number of ants of the same colony within the neighbourhood.
   *
   * The size of the observed neighbourhood is indicated by `antsSensingRange`.
   *
   * @return Number of ants of the same colony in the neighbourhood
   */
  def countFriends(): Int =
    world.antsInNeighbourhoodOf(pos = currentPos, range =  antsSensingRange).count(a => a.tribeID == this.tribeID)

  /**
   * Counts the number of ants of other colonies within the neighbourhood.
   *
   * The size of the observed neighbourhood is indicated by `antsSensingRange`.
   *
   * @return Number of ants of other colonies in the neighbourhood
   */
  def countStrangers(): Int =
    world.antsInNeighbourhoodOf(pos = currentPos, range = antsSensingRange).count((a: Ant) => a.tribeID != this.tribeID)


  //------------------------ Other common functions and procedures for ants -------------------------------


  /**
   * Chooses a direction to go to.
   *
   * Works in the following way: With a probability of (1 - `explorationRate`) the (valid) direction with
   * the best evaluation is chosen. In the other case there will be chosen a random direction.
   *
   * @param evaluate Function to evaluate
   * @return Direction chosen
   */
  protected def chooseDirectionBy(evaluate: Option[Direction.Value] => Double): Option[Direction.Value] = {
    val directionsValued: List[(Direction.Value, Double)] =
      accsessibleDirections.map(dir => (dir, evaluate(Some(dir)))) // Add to every direction its value

    val valDirsSorted = directionsValued.sortBy(x => x._2).reverse // descending order

    if (valDirsSorted.isEmpty)
      None
    else if (world.random.nextDouble() <= 1.0d - explorationRate)
      Some(valDirsSorted.head._1)
    else
      Some(valDirsSorted.apply(1 + world.random.nextInt(valDirsSorted.size - 1))._1)
  }

  // Calculates an all over all value for a direction
  protected def valueDirectionWithFunction
    (f: Option[Direction.Value] => Double)(oDir: Option[Direction.Value]): Double = {

    // Calculates a normalized value of a direction influenced by the pheromone
    def dirValueByPhero(oDir: Option[Direction.Value]): Double = {
      val someDirs: List[Option[Direction.Value]] = validDirections.map(Some.apply)
      val bestPheroInNeighbourhood = someDirs.map(f).max

      if (bestPheroInNeighbourhood == 0) 0 else f(oDir) / bestPheroInNeighbourhood
    }

    // Calculates a normalized value of a direction influenced by the last direction
    def dirValueByDir(oDir: Option[Direction.Value]): Double =
      oDir match {
        case None => 1
        case Some(dir) => Direction.directionDistance(lastDirection, dir) / Direction.MaxDirDistance
      }

    alpha * dirValueByPhero(oDir) + (1 - alpha) * dirValueByDir(oDir)
  }
}
