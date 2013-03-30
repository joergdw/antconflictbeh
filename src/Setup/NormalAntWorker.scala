package Setup

import sim.engine.SimState
import sim.util.Bag

final class NormalAntWorker(override val tribe: Tribe) extends AntWorker(tribe) {

  ///////////////////// Common variables and constants /////////////////////////////////////

  /**
   * Descripes the range of the emotions. Value must be odd and dividable by 3.
   *
   * Value determines the possible values of the variable `emotion`.
   * It's range is in [-(`emotionalRange` div 2), (`emotionalRange` div 2)]
   * `emotion` in the lower third of the scala indicates that the ant is the defensive state,
   * the middle third in the neutral state and the upper third in the aggressive state.
   */
  val emotionalRange: Int = 15
  private var emotion: Int = 0

  val sectionSize:Int = emotionalRange / 3  /** Size of each emotional state on the scala */
  val neutralLowerBound: Int = - (emotionalRange / 2) + sectionSize /** Lower bound of the neutral state */
  val neutralUpperBound: Int = emotionalRange / 2 - sectionSize /** Upper bound of the neutral state */

  val antsSensingRange: Int = 2 /** Radius of the area the ant can sense other individuals */


  ///////////////////// Basic operations /////////////////////////////////////

  override def receiveHit(opponent: AntWorker) {
    super.receiveHit(opponent)
    if (this.hitpoints == 0) return // Ant dead: no more actions

    if (neutralLowerBound <= emotion && emotion <= neutralUpperBound) // If ant neutral…
      adaptState() // … calculate new state
  }

  /**
   * Adapts the emotional state of the ant.
   *
   * Changes either to aggressive state or to defensive state, in function of the number of ants
   * of the same colony in the nearby environment.
   */
  def adaptState() {
     // TODO: Implementierung
  }

  /**
   * Counts the number of ants of the same colony within the neighbourhood.
   *
   * The size of the observed neighbourhood is indicated by `antsSensingRange`.
   *
   * @return Number of ants of the same colony
   */
  def countFriends(): Int = {
    // Get the bag with the objects
    val (xBag, yBag) = nearPosBags(antsSensingRange)
    val objects: Bag = new Bag()
    sim.ants.getObjectsAtLocations(xBag, yBag, objects)

    // Count the ones belonging to the same colony
    var counter = 0
    for (i <- 0 until objects.size()) {
      val ant = objects.get(i).asInstanceOf[Ant]
      if (ant.tribe.tribeID == this.tribe.tribeID)
        counter += 1
    }

    counter
  }


  ///////////////////// Behaviour description /////////////////////////////////////

  override def step(state: SimState) {
    // TODO: Add more behaviour
    actEconomically(state)
  }

  override def actMilitarily(state: SimState) {}
}