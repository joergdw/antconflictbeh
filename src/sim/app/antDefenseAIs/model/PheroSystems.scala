/*
 * Copyright © 2013 by Jörg D. Weisbarth
 *
 * ant program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License 3 as published by
 * the Free Software Foundation;
 *
 * ant program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY.
 *
 * See the License.txt file for more details.
 */

package sim.app.antDefenseAIs.model

/**
 * Implements the pheromone logic
 */
trait PheroSystem {
  val ant: Ant
  val world: World = ant.world

  import world._

  //---------------------------- Primitives ------------------------------------------------

  /**
   * Home pheromone intensity of the tribe of the ant in the given direction
   *
   * @param dir Direction where to investigate the pheromone intensity
   * @return Home pheromone intensity of the tribe of the ant in the given direction
   */
  protected[model] def homePheroOf(dir: Direction.Value): Double = world.homePheroOf(ant, dir).get

  /**
   * Home pheromone intensity of the tribe of the ant at its current position
   *
   * @return Home pheromone intensity of the tribe of the ant at its current position
   */
  protected[model] def homePheroOf(): Double = world.homePheroOf(ant).get

  /**
   * Resource pheromone intensity of the tribe of the ant in the given direction
   *
   * @param dir Direction where to investigate the pheromone intensity
   * @return Resource pheromone intensity of the tribe of the ant in the given direction
   */
  protected[model] def resPheroOf(dir: Direction.Value): Double = world.resPheroOf(ant, dir).get

  /**
   * Resource pheromone intensity of the tribe of the ant at its current position
   *
   * @return Resource pheromone intensity of the tribe of the ant at its current position
   */
  protected[model] def resPheroOf(): Double = world.resPheroOf(ant).get

  /**
   * War pheromone intensity of the tribe of the ant in the given direction
   *
   * @param dir Direction where to investigate the pheromone intensity
   * @return War pheromone intensity of the tribe of the ant in the given direction
   */
  protected[model] def warPheroOf(dir: Direction.Value): Double = world.warPheroOf(ant, dir).get

  /**
   * War pheromone intensity of the tribe of the ant at its current position
   *
   * @return Resource pheromone intensity of the tribe of the ant at its current position
   */
  protected[model] def warPheroOf(): Double = world.warPheroOf(ant).get

  /**
   * Set home pheromone intensity of the tribe of the ant at its current position
   *
   * @param intensity New intensity
   */
  protected[model] def setHomePhero(intensity: Double) {
    world.setHomePheroOn(ant, currentPosOf(ant).get, intensity)
  }

  /**
   * Set resource pheromone intensity of the tribe of the ant at its current position
   *
   * @param intensity New intensity
   */
  protected[model] def setResPhero(intensity: Double) {
    world.setResPheroOn(ant, currentPosOf(ant).get, intensity)
  }

  /**
   * Set war pheromone intensity of the tribe of the ant at its current position
   *
   * @param intensity New intensity
   */
  protected[model] def setWarPhero(intensity: Double) {
    world.setWarPheroOn(ant, currentPosOf(ant).get, intensity)
  }


  //-------------------------------- Adaption procedures ---------------------------------------

  /**
   * Adapts the home-pheromones of the current field in function of the pheromones on the neighbour fields.
   */
  protected[model] def adaptHomePhero()

  /**
   * Adapts the resource-pheromones of the current field in function of the pheromones on the neighbour fields.
   */
  protected[model] def adaptResPhero()

  /**
   * Adapts the war pheromones of the current field in function of the pheromones on the neighbour fields.
   */
  protected[model] def adaptWarPhero()

  /**
   * Adapts all pheromones of the current field in function of the pheromones on the neighbour fields.
   */
  protected[model] def adaptAllPheros() {
    adaptHomePhero()
    adaptResPhero()
    adaptWarPhero()
  }
}



/**
 * Implements the pheromone logic in a similar way as presented in the mentioned paper
 *
 * @param ant Ant equipped with this pheromone system
 * @param gamma Learning parameter for adaption of pheromones, according the one used paper
 */
class StandardPheroSystem(
   override val ant: Ant,
   val gamma: Double = 0.98d) extends PheroSystem {

  import java.lang.StrictMath.{min, max}
  import world.{validDirections, currentPosOf, resOn, maxResAmount}

  override protected[model] def adaptHomePhero() {
    val bestNeighbour: Direction.Value = validDirections(ant).get.sortBy(homePheroOf).reverse.head

    val adaptedValue = currentPosOf(world.queenOf(ant)) match {
      case None => 0 // queen is killed an there is no home
      case Some(qPos) if currentPosOf(ant) == qPos => 1.0d
      case _ => gamma * homePheroOf(bestNeighbour)
    }

    // To avoid pheromone value > 1 and worse value than before
    setHomePhero(min(1, max(homePheroOf(), adaptedValue)))
  }

  override protected[model] def adaptResPhero() {
    val bestNeighbour: Direction.Value = validDirections(ant).get.sortBy(resPheroOf).reverse.head
    val adaptedValue = resOn(currentPosOf(ant).get) + gamma * resPheroOf(bestNeighbour) / maxResAmount

    setResPhero(min(1, adaptedValue))
  }

  override protected[model] def adaptWarPhero() {
    val bestNeighbour: Direction.Value = validDirections(ant).get.sortBy(warPheroOf).reverse.head
    val adaptedValue = gamma * warPheroOf(bestNeighbour)

    setWarPhero(min(warPheroOf(), adaptedValue))
  }
}