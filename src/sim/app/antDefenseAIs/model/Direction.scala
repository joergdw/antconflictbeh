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

package sim.app.antDefenseAIs.model

import sim.app.antDefenseAIs.common.Common.mannheimDistance
import scala.collection.immutable.HashMap

/**
 * Offers methods and names for directions
 */
private[model] object Direction extends Enumeration {

  val NorthWest = Value("north west") /** Direction north west */
  val North = Value("north") /** Direction north */
  val NorthEast = Value("north east") /** Direction north east */
  val West = Value("west") /** Direction west */
  val Place = Value("no direction") /** No direction (current place) */
  val East = Value("east") /** Direction east */
  val SouthWest = Value("south west") /** Direction south-west */
  val South = Value("south") /** Direction south */
  val SouthEast = Value("south east") /** Direction south west */

  /* Internal associations between direction and element of Z_3 x Z_3 which is used to compare a distance between
   * two directions. It should be like that, that the opposite direction of a given direction has the highest distance.
   *
   * Important knowledge for Mason topology: Field (x, y) is in column x, row y.
   */
  private val assocs = HashMap(
      (NorthWest, (-1, -1)),
      (North, (0, -1)),
      (NorthEast, (1, -1)),
      (West, (-1, 0)),
      (Place, (0, 0)),
      (East, (1, 0)),
      (SouthWest, (-1, 1)),
      (South, (0, 1)),
      (SouthEast, (1, 1))
    )

  /**
   * Is a degree for the difference of directions
   *
   * @return difference of two directions
   */
  def directionDifference(dir1: Value, dir2: Value): Int =
    mannheimDistance(assocs.get(dir1).get, assocs.get(dir2).get)

  def goDirection(pos: (Int, Int), dir: Value): (Int, Int) = {
    val (x, y) = assocs.get(dir).get
    (pos._1 + x, pos._2 + y)
  }
}
