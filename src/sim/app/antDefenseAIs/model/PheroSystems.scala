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
   * Home pheromone intensity of the tribe of the ant in the given direction, if given. On the current position
   * otherwise.
   *
   * @param oDir Direction where to investigate the pheromone intensity
   * @return Home pheromone intensity of the tribe of the ant in the given direction
   */
  protected[model] def homePheroOf(oDir: Option[Direction.Value]): Double =
    oDir match {
      case None => world.homePheroOf(this).get
      case Some(dir) => world.homePheroOf(this, dir).get
    }

  /**
   * Resource pheromone intensity of the tribe of the ant in the given direction, if given. On the current position
   * otherwise.
   *
   * @param oDir Direction where to investigate the pheromone intensity
   * @return Home pheromone intensity of the tribe of the ant in the given direction
   */
  protected[model] def resPheroOf(oDir: Option[Direction.Value]): Double =
    oDir match {
      case None => world.resPheroOf(this).get
      case Some(dir) => world.resPheroOf(this, dir).get
    }

  /**
   * War pheromone intensity of the tribe of the ant in the given direction, if given. On the current position
   * otherwise.
   *
   * @param oDir Direction where to investigate the pheromone intensity
   * @return Home pheromone intensity of the tribe of the ant in the given direction
   */
  protected[model] def warPheroOf(oDir: Option[Direction.Value]): Double =
    oDir match {
      case None => world.warPheroOf(this).get
      case Some(dir) => world.warPheroOf(this, dir).get
    }

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


  //------------------------------- Other stuff ----------------------------------

  /**
   * Gradient for the given pheromone map
   *
   * @param f Maps direction to a pheromone value
   * @return None, if no neighbour field has a higher value and Some direction otherwise
   */
  def gradientOf(f: Option[Direction.Value] => Double): Option[Direction.Value] = {
    val current = (None, f(None)): (Option[Direction.Value], Double)
    var best = current
    for (dir <- validDirections) {
      val tmp = (Some(dir), f(Some(dir)))
      if (tmp._2 > best._2)
        best = tmp
    }

    best._1
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
    val someDirs: List[Option[Direction.Value]] = validDirections.map(Some.apply)
    val bestNeighbour: Direction.Value = someDirs.sortBy(homePheroOf).reverse.head.get

    val adaptedValue = currentPosOf(myQueen) match {
      case None => 0 // queen is killed an there is no home
      case Some(qPos) if currentPos == qPos => 1.0d
      case _ => gamma * homePheroOf(Some(bestNeighbour))
    }

    // To avoid pheromone value > 1 and worse value than before
    setHomePhero(min(1, max(homePheroOf(None), adaptedValue)))
  }

  override protected[model] def adaptResPhero() {
    val someDirs = validDirections.map(Some.apply)
    val bestNeighbour: Direction.Value = someDirs.sortBy(resPheroOf).reverse.head.get
    val adaptedValue = resOn(currentPos) + gamma * resPheroOf(Some(bestNeighbour)) / maxResAmount

    setResPhero(min(1, adaptedValue))
  }

  override protected[model] def adaptWarPhero() {
    val someDirs = validDirections.map(Some.apply)
    val bestNeighbour: Direction.Value = someDirs.sortBy(warPheroOf).reverse.head.get
    val adaptedValue = gamma * warPheroOf(Some(bestNeighbour))

    setWarPhero(min(warPheroOf(None), adaptedValue))
  }
}