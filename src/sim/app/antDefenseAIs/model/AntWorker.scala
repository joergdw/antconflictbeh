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
 *
 * @param tribeID Tribe the ant belongs to
 * @param world World the ant lives on
 */
private[antDefenseAIs] abstract class AntWorker(
  override val tribeID: Int,
  override val world: World) extends Ant(tribeID, world) {

  ///////////////////// Common variables and constants /////////////////////////////////////

  protected var transporting: Int = 0 /** Amount of resources transported by this ant */
  override final def maximumAge(): Int = AntWorker.maximumAge


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
      val res = world.resOn(pos.get) + transporting
      world.setResOn(pos.get, res)
    }

    transporting = 0
  }

  /**
   * Mines, if possible, resources.
   */
  def mineRes() {
    val pos = currentPos.get
    val spaceLeft: Boolean = backpackSize > transporting // space left in bag?
    if (spaceLeft && world.resOn(pos) > 0) {
      world.setResOn(pos, world.resOn(pos) - 1)
      transporting += 1
    }
  }


  //////////////////////// Information revealing functions for other classes ////////////

  /**
   * Amount of resources transported by this ant
   *
   * @return Amount of resources transported by this ant
   */
  final def inBackpack: Int = transporting
}
