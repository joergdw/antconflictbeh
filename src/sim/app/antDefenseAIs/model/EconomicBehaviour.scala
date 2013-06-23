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

/**
 *
 */
trait EconomicBehaviour {

  /**
   * Actions for ants serving the economy of its tribe.
   */
  protected[model] def actEconomically()
}



trait EconomicStandardBehaviour extends AntWorker with EconomicBehaviour with PheroSystem {
  val notBored: Int /** Value of boredom if the ant is not bored at all */

  protected[model] var boredom: Int = notBored /** 0 if an ant is „bored“ of searching abortively food and wants to go home */
  protected[model] def isBored: Boolean = boredom == 0

  import world.currentPosOf

  /** Actions for ants serving the economy of its tribe.
    *
    * If the backpack is full, or the ant is bored that is the ant has searched too long resources
    * without success, the ant follows the way home to its queen and give all resources in the backpack
    * to her. (After that ant is not bored at all.)
    *
    * In any other case the ant cares for food.
    */
  protected[model] def actEconomically() {
    val backpack_full: Boolean = inBackpack >= AntWorker.backpackSize

    if (backpack_full || isBored) {
      val queenPos: Option[(Int, Int)] = currentPosOf(myQueen)
      if (queenPos.isDefined && currentPos == queenPos.get) { // queen is under the ant
        dropResources()
        boredom = notBored
      }
      else
        followHomeWay()
    }
    else
      careForFood()
  }

  /**
   * Care for food.
   *
   * The next field is most probable one of the neighbour-fields with the best resource-pheromones.
   * With a certain probability (in function of the world.explorationRate) it is one of the other fields
   */
  protected[model] def careForFood() {
    val direction = chooseDirectionBy(valueDirectionWithFunction(resPheroOf))
    if (direction.isDefined) {
      moveTo(direction.get)
      adaptAllPheros()
      mineRes()
    }
  }
}
