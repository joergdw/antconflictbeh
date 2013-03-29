package Setup

import sim.engine.SimState

final class NormalAntWorker(override val tribe: Tribe) extends AntWorker(tribe) {

  override def receiveHit(opponent: AntWorker) {
    hitpoints = hitpoints - opponent.attack

    // Ant should die if no hitpoints left and drop resources
    if (this.hitpoints == 0) {
      dropResources()
      sim.ants.remove(this)
      // TODO: Necessary to take ant out of scheduling?

    } else if (aggressivity > 0) // If aggressive, ant should stay aggressive
      aggressivity = aggressivityRange
  }

  /** actions when ant want to fight – dependent of the ant-type */
  override def actMilitarily(state: SimState) {}

  override def step(state: SimState) {
    // TODO: Add more behaviour
    actEconomically(state)
  }
}