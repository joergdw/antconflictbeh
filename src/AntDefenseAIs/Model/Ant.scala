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
package AntDefenseAIs.Model

import sim.engine.Steppable

/**
 * AntDefenseAIs.Common properties and capacities of ants
 *
 * @param tribeID Tribe the ant is member of
 * @param world World the ant lives on
 */
private[AntDefenseAIs] abstract class Ant(val tribeID: Int, val world: World) extends Steppable {

  protected val attack: Int = 1 /** Damage an ant does to another */
  protected var hitpoints: Int = 10 /** How much an individual can suffer before dieing */
  protected val mobility: Float = 0.5f /** Probability to avoid to be hit */

  /**
   * Current position of that ant as (Int, Int)
   *
   * @return Current position of that ant as (Int, Int)
   */
  def currentPos: (Int, Int) = world.currentPos(this)

  /**
   * Returns the positions of all the fields within a given distance. Current position of the ant is excluded.
   *
   * @param distance Maximum distance of a field which coordinates are included in the result
   * @return Positions within a given distance to the ant; Current position of the ant is excluded
   */
  protected def nearPos(distance: Int) = world.nearPos(this, distance)

  /**
   * Returns the positions of all the fields within a given distance.
   *
   * @param distance Maximum distance of a field which coordinates are included in the result
   * @return Positions within a given distance to the ant
   */
  protected def neighbourhood(distance: Int) = world.neighbourhood(this, distance)

  /**
   * Moves ant to another position. New position must be a neighbour position.
   *
   * @param newPos New position of the ant
   */
  protected def moveTo(newPos: (Int, Int)) {world.moveTo(this, newPos)}

  /**
   * Returns a reference to the queen of the ant
   *
   * @return Queen of the ant
   */
  private[Model] def myQueen: AntQueen = world.queenOf(this)

  /**
   * Home pheromone intensity of the tribe of the ant at the given position
   *
   * @param pos Position where to investigate the pheromone intensity
   * @return Home pheromone intensity of the tribe of the ant at the given position
   */
  protected def homePheroOn(pos: (Int, Int)) = world.homePheroOn(this, pos)

  /**
   * Resource pheromone intensity of the tribe of the ant at the given position
   *
   * @param pos Position where to investigate the pheromone intensity
   * @return Resource pheromone intensity of the tribe of the ant at the given position
   */
  protected def resPheroOn(pos: (Int, Int)) = world.resPheroOn(this, pos)

  /**
   * War pheromone intensity of the tribe of the ant at the given position
   *
   * @param pos Position where to investigate the pheromone intensity
   * @return War pheromone intensity of the tribe of the ant at the given position
   */
  protected def warPheroOn(pos: (Int, Int)) = world.warPheroOn(this, pos)

  /**
   * Set home pheromone intensity of the tribe of the ant at the given position
   *
   * @param pos Position where to set the pheromone intensity
   * @param amount New intensity
   */
  protected def setHomePheroOn(pos: (Int, Int), amount: Int) {
    world.setHomePheroOn(this, pos, amount)
  }

  /**
   * Set resource pheromone intensity of the tribe of the ant at the given position
   *
   * @param pos Position where to set the pheromone intensity
   * @param amount New intensity
   */
  protected def setResPheroOn(pos: (Int, Int), amount: Double) {
    world.setResPheroOn(this, pos, amount)
  }

  /**
   * Set war pheromone intensity of the tribe of the ant at the given position
   *
   * @param pos Position where to set the pheromone intensity
   * @param amount New intensity
   */
  protected def setWarPheroOn(pos: (Int, Int), amount: Double) {
    world.setWarPheroOn(this, pos, amount)
  }

  /**
   * True if the field on position pos returns at least one enemy
   *
   * @param position Position to be searched for enemies
   * @return True iff the field on the given position contains at least one enemy
   */
  def enemySensedOn(position: (Int, Int)): Boolean = {
    def predicate(b: Boolean, a: Ant): Boolean = b || a.tribeID != this.tribeID
    world.antsOn(position).foldLeft(false: Boolean)(predicate)
  }
}
