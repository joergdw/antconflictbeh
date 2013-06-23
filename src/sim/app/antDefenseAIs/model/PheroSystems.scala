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
trait PheroSystem extends Ant {



  //---------------------------- Primitives ------------------------------------------------

  /**
   * Home pheromone intensity of the tribe of the ant in the given direction
   *
   * @param dir Direction where to investigate the pheromone intensity
   * @return Home pheromone intensity of the tribe of the ant in the given direction
   */
  protected[model] def homePheroOf(dir: Direction.Value): Double = world.homePheroOf(this, dir).get

  /**
   * Home pheromone intensity of the tribe of the ant at its current position
   *
   * @return Home pheromone intensity of the tribe of the ant at its current position
   */
  protected[model] def homePheroOf(): Double = world.homePheroOf(this).get

  /**
   * Resource pheromone intensity of the tribe of the ant in the given direction
   *
   * @param dir Direction where to investigate the pheromone intensity
   * @return Resource pheromone intensity of the tribe of the ant in the given direction
   */
  protected[model] def resPheroOf(dir: Direction.Value): Double = world.resPheroOf(this, dir).get

  /**
   * Resource pheromone intensity of the tribe of the ant at its current position
   *
   * @return Resource pheromone intensity of the tribe of the ant at its current position
   */
  protected[model] def resPheroOf(): Double = world.resPheroOf(this).get

  /**
   * War pheromone intensity of the tribe of the ant in the given direction
   *
   * @param dir Direction where to investigate the pheromone intensity
   * @return War pheromone intensity of the tribe of the ant in the given direction
   */
  protected[model] def warPheroOf(dir: Direction.Value): Double = world.warPheroOf(this, dir).get

  /**
   * War pheromone intensity of the tribe of the ant at its current position
   *
   * @return Resource pheromone intensity of the tribe of the ant at its current position
   */
  protected[model] def warPheroOf(): Double = world.warPheroOf(this).get

  /**
   * Set home pheromone intensity of the tribe of the ant at its current position
   *
   * @param intensity New intensity
   */
  protected[model] def setHomePhero(intensity: Double) {
    world.setHomePheroOn(this, currentPos, intensity)
  }

  /**
   * Set resource pheromone intensity of the tribe of the ant at its current position
   *
   * @param intensity New intensity
   */
  protected[model] def setResPhero(intensity: Double) {
    world.setResPheroOn(this, currentPos, intensity)
  }

  /**
   * Set war pheromone intensity of the tribe of the ant at its current position
   *
   * @param intensity New intensity
   */
  protected[model] def setWarPhero(intensity: Double) {
    world.setWarPheroOn(this, currentPos, intensity)
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


  //---------------------------- Procedures making use of the pheromone system -----------------

  /**
   * Follow home way.
   *
   * The next field is most probable one of the neighbour-fields with the best home-pheromones.
   * With a certain probability (in function of the world.explorationRate) it is one of the other fields.
   */
  protected[model] def followHomeWay() {
    val direction = chooseDirectionBy(valueDirectionWithFunction(homePheroOf))
    if (direction.isDefined) {
      moveTo(direction.get)
      adaptAllPheros()
    }
  }
}



/**
 * Implements the pheromone logic in a similar way as presented in the mentioned paper
 */
trait StandardPheroSystem extends PheroSystem {

  val gamma: Double // Learning parameter for adaption of pheromones, according the one used paper

  import java.lang.StrictMath.{min, max}
  import world.{currentPosOf, resOn, maxResAmount}

  override protected[model] def adaptHomePhero() {
    val bestNeighbour: Direction.Value = validDirections.sortBy(homePheroOf).reverse.head

    val adaptedValue = currentPosOf(myQueen) match {
      case None => 0 // queen is killed an there is no home
      case Some(qPos) if currentPos == qPos => 1.0d
      case _ => gamma * homePheroOf(bestNeighbour)
    }

    // To avoid pheromone value > 1 and worse value than before
    setHomePhero(min(1, max(homePheroOf(), adaptedValue)))
  }

  override protected[model] def adaptResPhero() {
    val bestNeighbour: Direction.Value = validDirections.sortBy(resPheroOf).reverse.head
    val adaptedValue = resOn(currentPos) + gamma * resPheroOf(bestNeighbour) / maxResAmount

    setResPhero(min(1, adaptedValue))
  }

  override protected[model] def adaptWarPhero() {
    val bestNeighbour: Direction.Value = validDirections.sortBy(warPheroOf).reverse.head
    val adaptedValue = gamma * warPheroOf(bestNeighbour)

    setWarPhero(min(warPheroOf(), adaptedValue))
  }
}