package Setup

import sim.engine.SimState

final class AntQueen(override val tribe: Tribe) extends Ant(tribe) {

  var deposit: Int = sim.startRessources /** resources the queen owns */      // TODO: Sicherstellen, dass hier niemand dran was dreht

  def productionTime: Int = sim.productionTime
  def productionCost: Int = sim.productionCost

  /*
  0 means: no ant being produced
  every other value between 1 and productionTime is the number of
  leaving time units until the production is completed
   */
  private var productionState: Int = 0

  /**
   * Used to give the queen resources
   *
   * @param amount amount of resources the queen receives
   */
  def receiveRes(amount: Int) = {
    assert(amount >= 0)

    deposit += amount
  }

  /**
   * Queen tries to create new ant.
   *
   * Success iff enough resources available and maximum population not reached
   */
  override def step(state: SimState) = {
    assert(0 <= productionState && productionState <= productionTime)

    val tmp = deposit - productionCost
    val curPop = sim.populationStat()(tribe.tribeID) // current population of that tribe

    if (tmp >= 0 && productionState == 0) { // enough resources and no other construction in progress?
      deposit = tmp
      productionState += 1
    }

    else if (productionState >= productionTime - 1   // production completed?
             && curPop < sim.maxPopulation) {
      productionState = 0

      val ant = new NormalAntWorker(tribe)
      sim.ants.setObjectLocation(ant, currentPosInt2D)
      sim.schedule scheduleRepeating(ant)
    }

    else if (productionState > 0 && productionState < productionTime - 1) // production started and not ready?
      productionState += 1  // advance in construction
  }
}