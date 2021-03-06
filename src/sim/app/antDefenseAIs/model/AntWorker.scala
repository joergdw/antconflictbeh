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

private[antDefenseAIs] object AntWorker {
  val maximumAge: Int = 10000 /** Maximum age of a ant (in steps) */
  val backpackSize: Int = 5 /** Amount of resources which can be transported by an individual */
}


import AntWorker._

/**
 * What have all ant workers have in common
 */
private[antDefenseAIs] abstract class AntWorker extends Ant with PheroSystem {


  //------------------- Common variables and constants -----------------------------------
  override final def maximumAge: Int = AntWorker.maximumAge


  //------------------- Basic operations of ant workers-----------------------------------

  /**
   * Drops the resources on the current place. If the queen is there, she
   * receives them.
   */
  final protected[model] def dropResources() {
    val pos = currentPos
    val qPos: Option[(Int, Int)] = world.currentPosOf(myQueen)
    if (qPos.isDefined && pos == qPos.get)
      myQueen.receiveRes(_inBackpack)
    else {
      val res = world.resOn(pos) + _inBackpack
      world.setResOn(pos, res)
    }

    _inBackpack = 0
  }

  /**
   * Mines, if possible, resources.
   */
  protected[model] def mineRes() {
    val pos = currentPos
    val spaceLeft: Boolean = backpackSize > _inBackpack // space left in bag?
    if (spaceLeft && world.resOn(pos) > 0) {
      world setResOn(pos, world.resOn(pos) - 1)
      _inBackpack += 1
    }
  }

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
