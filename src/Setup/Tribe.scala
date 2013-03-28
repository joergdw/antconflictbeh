package Setup

import sim.field.grid.{IntGrid2D, DoubleGrid2D}
import sim.engine.{SimState, Steppable}

/** Shared information and data between all the members of a tribe
 *
 * @param tribeID Identifier for the tribe the ant belongs to
 * @param simulation Simulation the tribe participates
 * @param homePhero Pheromone-map of the tribe for going home
 *@param resPhero Pheromone-map of the tribe for searching food
 * @param warPhero Pheromone-map of the tribe for war-communication
 */
final class Tribe(val tribeID: Int,
                  val simulation: Simulation,
                  val homePhero: IntGrid2D,
                  val resPhero: DoubleGrid2D,
                  val warPhero: DoubleGrid2D) extends Steppable {

  /**
   * Queen of that tribe
   *
   * Design decision: it's a variable to enable the change of a queen in the case that ant-aging is introduced
   */
  var queen: AntQueen = null

  /** Adapts the pheromone maps in function of the time (diffusion, evaporation)
   *
   * @param state
   */
  def step(state: SimState) = {}
}
