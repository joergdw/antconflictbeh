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

import scala.collection.immutable.HashMap

/**
 * Modelling directions and its internals.
 *
 * Dependent of implementation of `ants` and therefore here.
 */
private[model] object Direction extends Enumeration {

  val SouthWest = Value("south west") /** Direction south-west */
  val West = Value("west") /** Direction west */
  val NorthWest = Value("north west") /** Direction north west */
  val North = Value("north") /** Direction north */
  val NorthEast = Value("north east") /** Direction north east */
  val East = Value("east") /** Direction east */
  val SouthEast = Value("south east") /** Direction south west */
  val South = Value("south") /** Direction south */

  /**
   * Distance of directions
   *
   * The opposite direction always has the highest distance.
   *
   * @return Distance of two directions
   */
  def directionDistance(dir1: Value, dir2: Value): Int = {
    import StrictMath.{abs, min}

    /* Directions are ordered with their values like in a circle of the ring Z_8 of eight elements.
     * Therefore their distance towards each other is their distance in Z_8.
     */
    val a = dir1.id; val b = dir2.id
    min(abs(a - b), abs(a - abs(values.size - b)))
  }

  val MaxDirDistance: Int = directionDistance(North, South) /** Maximum distance of two directions */


  /* Internal associations between direction and element of Z_3 x Z_3 which is used to generate
   * a new position out of a given position and a direction
   * two directions. It should be like that, that the opposite direction of a given direction has the highest distance.
   *
   * Important knowledge for Mason topology: Field (x, y) is in column x, row y.
   */
  private val assocs = HashMap(
      NorthWest -> (-1, -1),
      North -> (0, -1),
      NorthEast -> (1, -1),
      West -> (-1, 0),
      East -> (1, 0),
      SouthWest -> (-1, 1),
      South -> (0, 1),
      SouthEast -> (1, 1)
    )

  private val assocsm1 = HashMap( // assocs inverse
    (-1, -1) -> NorthWest,
    (0, -1) -> North,
    (1, -1) -> NorthEast,
    (-1, 0) -> West,
    (1, 0) -> East,
    (-1, 1) -> SouthWest,
    (0, 1) -> South,
    (1, 1) -> SouthEast
  )

  /**
   * The first position in direction `oDir` from position `pos`
   *
   * @param pos Start position
   * @param dir Direction to go to
   * @return First position in direction `oDir` from position `pos`
   */
  def inDirection(pos: (Int, Int), dir: Value): (Int, Int) = {
    val (x, y) = assocs(dir)
    (pos._1 + x, pos._2 + y)
  }

  /**
   * Given two neighbour positions a direction from the first to the second will be calculated.
   *
   * @param start Start position
   * @param target Target position
   * @return Direction between start and target position
   */
  def directionIs(start: (Int, Int), target: (Int, Int)): Direction.Value = {
    val diff = (target._1 - start._1, target._2 - start._2)
    assocsm1(diff)
  }
}
