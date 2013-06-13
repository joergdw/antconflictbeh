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
  val maximumHitpoints = 5 /** How many hitpoints an undamaged individual has */
}


import sim.engine.Steppable
import Ant.maximumHitpoints

/**
 * Common properties and capacities of ants
 *
 * @param tribeID Tribe the ant is member of
 * @param world World the ant lives on
 */
private[antDefenseAIs] abstract class Ant(
  val tribeID: Int,
  val world: World) extends Steppable {

  protected final var hitpoints: Int = maximumHitpoints /** How much an individual can suffer before dieing */
  protected final var mobility: Float = 0f /** Probability to avoid to be hit */
  protected final var attack: Int = 1 /** Damage an ant does to another */
  private[model] final var age: Int = 0 /** Current age of the ant */
  protected[this] final var _inBackpack: Int = 0 /** Amount of resources transported by this ant */

  /** Last went direction */
  protected final var lastDirection: world.Direction.Value = {  // Initialise with random value
    val valueList = world.Direction.values.toList
    valueList.apply(world.random.nextInt(world.Direction.values.size))
  }

  /**
   * Amount of resources transported by this ant
   *
   * @return Amount of resources transported by this ant
   */
  def inBackpack(): Int = _inBackpack

  def maximumAge(): Int /** Maximum age of an ant */

  /**
   * Current position of that ant as (Int, Int)
   *
   * @return Current position of that ant as (Int, Int)
   */
  protected[this] def currentPos: (Int, Int) = world.currentPosOf(this).get

  /**
   * All directions in which the ant can go right now
   *
   * @return All directions in which the ant can go right now
   */
  protected[this] def validDirections = world.validDirections(this).get

  /**
   * Moves ant into the given direction
   *
   * @param direction New position of the ant
   */
  protected[this] def moveTo(direction: world.Direction.Value) {
    world move(this, direction)
    lastDirection = direction
  }

  /**
   * Returns a reference to the queen of the ant
   *
   * @return Queen of the ant
   */
  private[model] def myQueen: AntQueen = world queenOf this

  /**
   * Home pheromone intensity of the tribe of the ant in the given direction
   *
   * @param dir Direction where to investigate the pheromone intensity
   * @return Home pheromone intensity of the tribe of the ant in the given direction
   */
  protected[this] def homePheroOf(dir: world.Direction.Value) = world.homePheroOf(this, dir).get

  /**
   * Home pheromone intensity of the tribe of the ant at its current position
   *
   * @return Home pheromone intensity of the tribe of the ant at its current position
   */
  protected[this] def homePheroOf() = world.homePheroOf(this).get

  /**
   * Resource pheromone intensity of the tribe of the ant in the given direction
   *
   * @param dir Direction where to investigate the pheromone intensity
   * @return Resource pheromone intensity of the tribe of the ant in the given direction
   */
  protected[this] def resPheroOf(dir: world.Direction.Value) = world.resPheroOf(this, dir).get

  /**
   * Resource pheromone intensity of the tribe of the ant at its current position
   *
   * @return Resource pheromone intensity of the tribe of the ant at its current position
   */
  protected[this] def resPheroOf() = world.resPheroOf(this).get

  /**
   * War pheromone intensity of the tribe of the ant in the given direction
   *
   * @param dir Direction where to investigate the pheromone intensity
   * @return War pheromone intensity of the tribe of the ant in the given direction
   */
  protected[this] def warPheroOf(dir: world.Direction.Value) = world.warPheroOf(this, dir).get

  /**
   * War pheromone intensity of the tribe of the ant at its current position
   *
   * @return Resource pheromone intensity of the tribe of the ant at its current position
   */
  protected[this] def warPheroOf() = world.warPheroOf(this).get

  /**
   * Set home pheromone intensity of the tribe of the ant at its current position
   *
   * @param intensity New intensity
   */
  protected[this] def setHomePhero(intensity: Double) {
    world setHomePheroOn(this, currentPos, intensity)
  }

  /**
   * Set resource pheromone intensity of the tribe of the ant at its current position
   *
   * @param intensity New intensity
   */
  protected def setResPhero(intensity: Double) {
    world setResPheroOn(this, currentPos, intensity)
  }

  /**
   * Set war pheromone intensity of the tribe of the ant at its current position
   *
   * @param intensity New intensity
   */
  protected def setWarPhero(intensity: Double) {
    world setWarPheroOn(this, currentPos, intensity)
  }


  /**
   * Adaptions after receiving a hit
   */
  private[model] def receiveHitFrom(opponent: Ant) {
    if (world.random.nextDouble() >= mobility) // If ant can ...
      hitpoints = hitpoints - attack
  }

  /**
   * Hit an opponent
   *
   * @param opponent Opponent receiving a hit
   */
  private[model] def hit(opponent: Ant) {
    world.hit(this)(opponent)
  }

  /**
   * Returns true if the ant is dead
   *
   * @return True iff ant is dead
   */
  final def isKilled: Boolean = hitpoints == 0

  /**
   * Counts the number of ants within the neighbourhood fulfilling a predicate.
   *
   * The size of the observed neighbourhood is indicated by `antsSensingRange`.
   *
   * @param range Range in which will be searched
   * @param p Predicate
   * @return Number of ants in the neighbourhood fulfilling the predicate p
   */
  def countAntsFullfillingPredicate(range: Int)(p: Ant => Boolean): Int = {
    val ants: List[Ant] = world.neighbourhood(this, range).get.map(world.antsOn).flatten
    def adder(i: Int, a: Ant): Int = i + (if (p(a)) 1 else 0)
    ants.foldLeft(0: Int)(adder)
  }
}
