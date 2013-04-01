/*
 * Copyright © 2012 - 2013 by Jörg D. Weisbarth <joerg.bretten@web.de>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License 3 as published by
 * the Free Software Foundation;
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY.
 *
 * See the License.txt file for more details.
 */
package Setup

import sim.engine.Steppable
import sim.util.Int2D
import Helpers.toTuple


abstract class Ant(val tribe: Tribe) extends Steppable {

  def sim = tribe.simulation

  /** current position of that ant as Int2D */
  def currentPosInt2D: Int2D = sim.ants.getObjectLocation(this)

  /** current position of that ant as (Int, Int) */
  def currentPos: (Int, Int) = toTuple(currentPosInt2D)
}
