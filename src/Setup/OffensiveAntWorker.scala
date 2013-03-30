package Setup

import sim.engine.SimState

class OffensiveAntWorker(override val tribe: Tribe) extends AntWorker(tribe) {

  override def receiveHit(opponent: AntWorker) {
    super.receiveHit(opponent)
    if (this.hitpoints == 0) return // Ant dead: no more actions
  }

  override def actMilitarily(state: SimState) {}

  def step(state: SimState) {
    // TODO: Add more behaviour
    actEconomically(state)
  }
}
