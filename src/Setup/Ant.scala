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
