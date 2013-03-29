package Setup

import sim.engine.SimState
import sim.field.grid.{IntGrid2D, SparseGrid2D}
import TribeFactory.generateTribe
import sim.util.{IntBag, Int2D}
import Helpers.toTupleList

final class Simulation(var s: Long) extends SimState(s) {


  val height: Int = 30 // Size of the field
  val width: Int = 30  // Size of the field
  val nTribes: Int = 2 /** Number of tribes */
  val gamma: Double = 0.9d /** Learning parameter according the one used paper */
  val explorationRate: Double = 0.2d
  val maxResAmount: Int = 5 /** Maximum number of resources on a field */
  val maxResPhero: Double = maxResAmount * (1 + gamma) /** Maximum number of resource pheromones on a field */
  val maxHomePhero: Int = Int.MaxValue
  val maxWarPhero: Double = 0.0d // TODO: Anpassen, sobald sie ins Spiel kommen
  val pheroThreshould: Double = 0.0001d
  val tribes: Array[Tribe] = new Array(nTribes)

  val maxPopulation: Int = Int.MaxValue /** Maximum tribe population */
  val startRessources: Int = 20 /** amount of res a tribe starts with */
  val productionTime: Int = 10 /** time to produce an ant*/
  val productionCost: Int = 1 /** costs to produce an ant */

  val ants: SparseGrid2D = new SparseGrid2D(height, width) // agents: multiples can be on one field
  val resources: IntGrid2D = new IntGrid2D(height, width, 0) // resource-spots

  override def start() {
    super.start()

    ///////////////// Setup of the maps //////////////////////////////

    /* A map-pattern consisting of some to the two queens symmetric resource-spots
     *
     * One line of resources goes from the one corner to the other.
     */
    for (i <- 0 until resources.getHeight; j <- 0 until resources.getWidth if i == j) {
      val brushWidth: Int = 1

      val xPos: IntBag = new IntBag()
      val yPos: IntBag = new IntBag()
      resources.getNeighborsMaxDistance(i, j, brushWidth, false, xPos, yPos)
      val list = toTupleList(xPos, yPos)
      for ((x, y) <- list)
        resources set(x, y, maxResAmount)
    }
    //////////////////// Setup of the ant colonies ////////////////////
    tribes(0) = generateTribe(this, new Int2D(0, 0))
    tribes(1) = generateTribe(this, new Int2D(ants.getHeight - 1, ants.getWidth - 1))
  }

  ///////////////////////// Statistic related stuff /////////////////////////////////

  /** Counts population of all tribes
   *
   * @return field i contains the total population of the tribe with the ID i
   */
  def populationStat(): Array[Int] = {
    val objects = ants.getAllObjects
    val result = new Array[Int](tribes.length)

    for (i <- 0 until objects.size()) {
      val ant = objects.get(i).asInstanceOf[Ant]
      result(ant.tribe.tribeID) += 1
    }

    result
  }

  /** Counts resources owned by the ant queen of each tribe
   *
   * @return field i contains the amount of resources the queen of tribe i has
   */
  def resourceStat(): Array[Int] = {
    val result = new Array[Int](tribes.length)
    for (i <- 0 until result.length) {
      result(i) = tribes(i).queen.deposit
    }

    result
  }

  /**
   *
   * @return field i contains the total amount of resources owned by ants of the tribe i
   */
  def totalResStat(): Array[Int] = {
    val objects = ants.getAllObjects
    val result = new Array[Int](tribes.length)

    for (i <- 0 until objects.size()) {
      val o = objects.get(i)

      if (o.isInstanceOf[AntWorker]) {
        val ant = o.asInstanceOf[AntWorker]
        result(ant.tribe.tribeID) += ant.transporting
      } else if (o.isInstanceOf[AntQueen]) {
        val ant = o.asInstanceOf[AntQueen]
        result(ant.tribe.tribeID) += ant.deposit
      }
    }

    result
  }


  /// Other Helpers

  def ressourceMap(): Array[Array[Int]] = {
    val result: Array[Array[Int]] = new Array[Array[Int]](resources.getHeight)
    for (i <- 0 until result.length) {
      result(i) = new Array[Int](resources.getWidth)

      for (j <- 0 until result(i).length) {
        result(i)(j) = resources.get(i, j)
      }
    }

    result
  }
}