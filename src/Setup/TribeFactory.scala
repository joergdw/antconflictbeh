package Setup

import sim.field.grid.{IntGrid2D, DoubleGrid2D}
import sim.util.Int2D

object TribeFactory {
  var IDOfLastTribe: Int = -1 /** ID of the last generated tribe */  // -1 so that 0 is the ID of the first tribe

  /** Generates a new tribe and builds it into the sim
   *
   * @param simulation sim the tribe should be placed in
   * @param location location of the tribes queen
   */
  def generateTribe(simulation: Simulation, location: Int2D): Tribe = {
    val homePheros: IntGrid2D = new IntGrid2D(simulation.ants.getHeight,
                                        simulation.ants.getWidth, Int.MaxValue) // TODO: change initial value when changing the home-finding-system
    homePheros.set(location.getX, location.getY, 0) // Queen position is 0

    val resPheros: DoubleGrid2D = new DoubleGrid2D(simulation.ants.getHeight,
                                        simulation.ants.getWidth, 0.0d)
    val warPheros: DoubleGrid2D = new DoubleGrid2D(simulation.ants.getHeight,
                                        simulation.ants.getWidth, 0.0d)

    IDOfLastTribe += 1
    val tribe = new Tribe(IDOfLastTribe, simulation, homePheros, resPheros, warPheros)
    val queen = new AntQueen(tribe)
    tribe.queen = queen
    simulation.ants.setObjectLocation(queen, location)
    simulation.schedule.scheduleRepeating(queen)

    tribe
  }
}
