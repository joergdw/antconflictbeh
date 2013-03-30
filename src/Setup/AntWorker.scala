package Setup

import sim.engine.SimState
import sim.util.{IntBag, Int2D}
import Setup.Helpers._
import StrictMath.{min, max}

/** What have all ants in common
 *
 * @param tribe tribe the ant belongs to
 */
abstract class AntWorker (override val tribe: Tribe) extends Ant(tribe) {

  ///////////////////// Common variables and constants /////////////////////////////////////

  val backpack: Int = 1 /** amount of resources which can be transported by an individual */
  val attack: Int = 1 /** damage an ant does to another */
  val mobility: Float = 0.5f /** probability to avoid to be hit */
  val notBored: Int = 100 /** value of boredom, 100 if an ant is not bored at all */

  var transporting: Int = 0 /** amount of resources transported by this ant */  // TODO: Sicherstellen, dass hier niemand was dreht
  protected var hitpoints: Int = 10 /** how much an individual can suffer before dieing */
  protected var boredom: Int = notBored /** 0 if an ant is „bored“ of searching abortively food and wants to go home */



  ///////////////////// Behaviour description ////////////////////////////////////////

  override def step(state: SimState)

  /** Actions for ants serving the economy of its tribe.
    *
    * If the backpack is full, or the ant is bored that is the ant has searched too long resources
    * without success, the ant follows the way home to its queen and give all resources in the backpack
    * to her. (After that ant is not bored at all.)
    *
    * In any other case the ant cares for food.
    */
  final def actEconomically(state: SimState) {

    val backpack_full: Boolean = transporting >= backpack
    val is_bored: Boolean = boredom == 0

    if (backpack_full || is_bored) {
      if (currentPos == tribe.queen.currentPos) { // queen is under the ant
        dropResources()
        boredom = notBored
      }
      else
        followHomeWay()

    }
    else
      careForFood()
  }

  /** Actions when ant want to fight or to flee – dependent of the ant-type */
  def actMilitarily(state: SimState)


  /** Follow home way.
    *
    * The next field is the neighbour-field with the best home-pheromones.
    * Neighbour fields without foreign-colony ants take precedence (to avoid enemy-contact).
    */
  final def followHomeWay() {
    val list: List[Int2D] = nearPos(1) sortBy homePheroOn
    val noEnemyList = list filterNot enemySensedOn
    val nextPos = if (noEnemyList isEmpty) list.head else noEnemyList.head

    moveTo(nextPos)
    adaptResPhero()
  }

  /** Care for food.
    *
    * The next field is ost probable the neighbour-field with the best resource-pheromones.
    * With a certain probability (in function of the sim.explorationRate) it is any of the
    * neighbour fields.
    */
  final def careForFood() {
    val list: List[Int2D] = (nearPos(1) sortBy resPheroOn).reverse
    val nextPos: Int2D = if (sim.random.nextDouble() <= (1.0d - sim.explorationRate))
                           list.head
                         else
                           list.apply(sim.random.nextInt(list.size))

    moveTo(nextPos)
    adaptHomePhero()
    adaptResPhero()
    mineRes()
  }

  /**
   * Adapts the home-pheromones of the current field.
   */
  final def adaptHomePhero() {
    val (x, y) = currentPos
    val currentValue = homePheroOn(Helpers.toInd2D(x, y))

    val sortedNeighbours =  nearPos(1) sortBy homePheroOn
    val bestNeighbour = homePheroOn (sortedNeighbours.head)

    val adaptedValue = min(currentValue, max(bestNeighbour, bestNeighbour + 1)) // To avoid arithmetic overflow and worse distance
    tribe.homePhero.set(x, y, adaptedValue)
  }

  /**
   * Adapts the ressource-pheromones of the current field.
   */
  final def adaptResPhero() {
    val bestNeighbour = (nearPos(1) sortBy resPheroOn).reverse.head
    val adaptedValue = (resOn(currentPosInt2D) + sim.gamma * resPheroOn(bestNeighbour)) / sim.maxResAmount

    val (x, y) = currentPos
    tribe.resPhero.set(x, y, min(1, adaptedValue))
  }


  ///////////////////// Helping functions for the section above ////////////////////

  /**
   * Calculates all the neighbour positions within a certain distance.
   * Current position is not included.
   *
   * @param distance Maximum distance of a field towards the current position of the ant
   * @return List of positions within the given range.
   */
  final def nearPos(distance: Int): List[Int2D] = {
    val (xBag, yBag) = nearPosBags(distance)
    toInt2DList(xBag, yBag) filterNot currentPosInt2D.equals
  }

  /**
   * Calculates in two bags the x-positions an the y-positions of the neighbourhood within a given range
   *
   * @param distance Maximum distance of a field towards the current position of the ant
   * @return Tuple of bags with the x and the corresponding y-positions
   */
  final def nearPosBags(distance: Int): (IntBag, IntBag) = {
    val (x, y) = currentPos
    val xBag: IntBag = new IntBag()
    val yBag: IntBag = new IntBag()
    sim.ants.getNeighborsMaxDistance(x, y, distance, false, xBag, yBag)
    (xBag, yBag)
  }

  final def homePheroOn(pos: Int2D): Int = tribe.homePhero.get(pos.getX, pos.getY)
  final def resPheroOn(pos: Int2D): Double = tribe.resPhero.get(pos.getX, pos.getY)
  final def warPheroOn(pos: Int2D): Double = tribe.warPhero.get(pos.getX, pos.getY)

  final def resOn(pos: Int2D): Int = sim.resources.get(pos.getX, pos.getY)


  //////////////////// Basic operations of ants //////////////////////////////////////

  final def moveTo(newPos: Int2D) {
    // ASSERT: newPos is neighbour position of currentPos
    sim.ants.setObjectLocation(this, newPos)
  }

  final def moveTo(newPos: (Int, Int)) {
    moveTo(toInd2D(newPos))
  }

  /** What happens if an ant receives a hit */
  def receiveHit(opponent: AntWorker) {
    hitpoints = hitpoints - opponent.attack

    // Ant should die if no hitpoints left and drop resources
    if (this.hitpoints == 0) {
      dropResources()
      sim.ants.remove(this) // Take ant out of scheduling
    }
  }

  /** Hit an opponent
    *
    * @param opponent Opponent receiving a hit
    */
  def hit(opponent: AntWorker) {
    opponent.receiveHit(this)
  }

  /** Drops the resources on the current place. If the queen is there, she
   * receives them.
   */
  final def dropResources() {
    val pos = currentPos
    if (pos == tribe.queen.currentPos)
      tribe.queen.receiveRes(transporting)
    else {
      val res = sim.resources.get(pos._1, pos._2) + transporting
      sim.resources.set(pos._1, pos._2, res)
    }

    transporting = 0
  }

  /** Mines, if possible, resources. Boredom increased if no resources.
    * No boredom if try successful. */
  final def mineRes() {
    val (x, y) = currentPos
    val spaceLeft: Boolean = backpack > transporting // space left in bag?
    if (spaceLeft && resOn(currentPosInt2D) > 0) {
      sim.resources.set(x, y, resOn(currentPosInt2D) - 1)
      transporting += 1
      boredom = notBored
    }
    else
      boredom -= 1
  }

  /** True if the field on position pos returns at least one enemy */
  final def enemySensedOn(pos: Int2D): Boolean = {
    // Iterate through all objects on that field.
    var result: Boolean = false
    val bag = sim.ants.getObjectsAtLocation(pos)

    if (bag != null) {  // If there are objects at pos
      for (i: Int <- 0 until bag.size()) {

        val obj: AnyRef = bag.get(i)
        if (! obj.isInstanceOf[AntQueen]) {
          val ant: AntWorker = obj.asInstanceOf[AntWorker]
          if (ant.tribe.tribeID != tribe.tribeID) // If at least one has another tribeID…
            result = true
        }
      }
    }

    result
  }
}
