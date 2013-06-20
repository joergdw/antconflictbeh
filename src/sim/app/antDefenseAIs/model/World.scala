/*
 * Copyright © 2013 by Jörg D. Weisbarth
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License 3 as published by
 * the Free Software Foundation;
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY.
 *
 * See the License.txt file for more details.
 */
package sim.app.antDefenseAIs.model

import StrictMath.max
import scala.collection.immutable.HashMap
import scala.collection.mutable

import sim.field.grid.{DoubleGrid2D, IntGrid2D, SparseGrid2D}
import sim.engine.{Stoppable, SimState, Steppable}
import sim.util.{Bag, IntBag}

import sim.app.antDefenseAIs.common.Common._
import sim.app.antDefenseAIs.setup.Experiment

/**
 * World
 *
 * Description of the world of the simulation. Contains maps, data of pheromones for the different ant colonies, the
 * ants themselves and offers procedures for them to interact with its world.
 *
 * The resource distribution can only be set one time.
 *
 * A world must be scheduled from its caller – for every single turn. That's necessary for performing regular
 * nature-like actions, like removing dead ants.
 *
 * @param experiment Simulation the world is related to
 * @param resources Distribution of the resources on the map
 * @param height Height of the map
 * @param width  Width of the map
 * @param maxPopulation Maximum tribe population
 * @param tribeTypes Constructors of the different types of the tribe
 * @param maxAntsPerField Maximum number of ants permitted on a field
 */
private[antDefenseAIs] final class World(
  val experiment: Experiment,
  val width: Int,
  val height: Int,
  val maxPopulation: Int = Int.MaxValue,
  private val startPositions: Array[(Int, Int)],
  val resources: IntGrid2D,
  val maxAntsPerField: Int = 4,
  private val tribeTypes: Array[AntGenerator]) extends Steppable {

  if (experiment.numberOfTribes != tribeTypes.length)
    throw new IllegalArgumentException("Not exactly as many colony types as colonies")
  if (startPositions.length != tribeTypes.length)
    throw new IllegalArgumentException("Not exactly as many start positions as colony types")

/** Maximum number of resources on a field */
  val maxResAmount: Int = {
    var result = 0

    for (i <- 0 until resources.getWidth; j <- 0 until resources.getHeight) {
      result = max(result, resources.get(i, j))
    }

    result
  }

  val random = experiment.random /** Random number generator */

  // ASSERT: all start positions in range of `height` and `width`

  val ants: SparseGrid2D = new SparseGrid2D(width, height) /** Agents: multiples can be on one field */

  /**
   * Modelling directions and its internals.
   *
   * Dependent of implementation of `ants` and therefore here.
   */
  private[model] object Direction extends Enumeration {

    val SouthWest = Value("south west") /** Direction south-west */
    val West = Value("west") /** Direction west */
    val NorthWest = Value("north west") /** Direction north west */
    val North = Value("north") /** Direction north */
    val NorthEast = Value("north east") /** Direction north east */
    val East = Value("east") /** Direction east */
    val SouthEast = Value("south east") /** Direction south west */
    val South = Value("south") /** Direction south */

    /**
     * Distance of directions
     *
     * The opposite direction always has the highest distance.
     *
     * @return Distance of two directions
     */
    def directionDistance(dir1: Value, dir2: Value): Int = {
      import StrictMath.{abs, min}

      /* Directions are ordered with their values like in a circle of the ring Z_8 of eight elements.
       * Therefore their distance towards each other is their distance in Z_8.
       */
      val a = dir1.id; val b = dir2.id
      min(abs(a - b), abs(a - abs(values.size - b)))
    }

    val MaxDirDistance: Int = directionDistance(North, South) /** Maximum distance of two directions */


    /* Internal associations between direction and element of Z_3 x Z_3 which is used to generate
     * a new position out of a given position and a direction
     * two directions. It should be like that, that the opposite direction of a given direction has the highest distance.
     *
     * Important knowledge for Mason topology: Field (x, y) is in column x, row y.
     */
    private val assocs = HashMap(
      NorthWest -> (-1, -1),
      North -> (0, -1),
      NorthEast -> (1, -1),
      West -> (-1, 0),
      East -> (1, 0),
      SouthWest -> (-1, 1),
      South -> (0, 1),
      SouthEast -> (1, 1)
    )

    private val assocsm1 = HashMap( // assocs inverse
      (-1, -1) -> NorthWest,
      (0, -1) -> North,
      (1, -1) -> NorthEast,
      (-1, 0) -> West,
      (1, 0) -> East,
      (-1, 1) -> SouthWest,
      (0, 1) -> South,
      (1, 1) -> SouthEast
    )

    /**
     * The first position in direction `dir` from position `pos`
     *
     * @param pos Start position
     * @param dir Direction to go to
     * @return First position in direction `dir` from position `pos`
     */
    def inDirection(pos: (Int, Int), dir: Value): (Int, Int) = {
      val (x, y) = assocs(dir)
      (pos._1 + x, pos._2 + y)
    }

    /**
     * Given two neighbour positions a direction from the first to the second will be calculated.
     *
     * @param start Start position
     * @param target Target position
     * @return Direction between start and target position
     */
    def directionIs(start: (Int, Int), target: (Int, Int)): Direction.Value = {
      val diff = (target._1 - start._1, target._2 - start._2)
      assocsm1(diff)
    }
  }

  /**
   * Info-Datatype containing all the necessary information associated with each colony
   *
   * @param initialStartPosition Start position of the colony at the beginning of the simulation
   * @param queen Queen of the associated colony
   * @param homePheromones Storage of the home-pheromones of the colony
   * @param resPheromones Storage of the resource-pheromones of the colony
   * @param warPheromones Storage of the war-pheromones of the colony
   * @param killed Number of ants being killed in conflicts
   * @param overaged Number of ants died due to overageing
   */
  private[antDefenseAIs] class ColonyInfo(
    val initialStartPosition: (Int, Int),
    val queen: AntQueen,
    val homePheromones: DoubleGrid2D,
    val resPheromones: DoubleGrid2D,
    val warPheromones: DoubleGrid2D,
    private var hasKilled: Int,
    var killed: Int,
    var overaged: Int) {

    /**
     * Current population of the colony
     *
     * @return Current population of the colony
     */
    def population(): Int = allAnts.count(a => a.tribeID == queen.tribeID)

    /**
     * Total amount of resources hold by the ants of a colony
     *
     * @return Total amount of resources hold by the ants of a colony
     */
    def resources(): Int = (allAnts filter (a => a.tribeID == queen.tribeID)).foldLeft(0)((i, a) => i + a.inBackpack())

    /**
     * Total amount of resources hold by the queen of a colony
     *
     * @return Total amount of resources hold by the queen of a colony
     */
    def deposit(): Int = queen inBackpack()

    /**
     * True iff the queen has survived until now
     *
     * @return True iff the queen has survived until now
     */
    def queenSurvived(): Boolean = (ants getObjectLocation queen) != null

    /**
     * The following two lists contain the numbers for the fight situations:
     * `hit_made` contains the situations where the ant gave a hit and
     * `hit_received` contains the situations where the ant received a hit.
     *
     * Every list consist of tuples. The first one is the number of ants of the own colony and the second one
     * of ants of the other colony taking part in the fight.
     */
    private[World] var _hit_made = List[(Int, Int)]()
    private[World] var _hit_gotten = List[(Int, Int)]()

    def hit_gotten() = _hit_gotten
    def hit_made() = _hit_made

    def hitsMade(): Int = _hit_made.size
    def hitsGotten(): Int = _hit_gotten.size
    def hitsIn(sit: ((Int, Int)) => Boolean)(l: List[(Int, Int)]): Int = l.count(sit)

    def averageMayoritySituationHitMade(): Option[(Int, Int)] = averageOf(_hit_made)(mayority)
    def averageMinoritySituationHitMade(): Option[(Int, Int)] = averageOf(_hit_made)(minority)

    // average hitpoints of the left ants
    def averageHitPoints() = {
      def summer: (Ant, Int) => Int = (a, i) => a.hitpoints() + i

      val l = allAnts.filter(a => a.tribeID == queen.tribeID)
      l.foldRight(0)(summer) / l.length
    }


    // ---------------------------------- Helpers --------------------------------

    // Returns None if no fight situations found
    private def averageOf(l: List[(Int, Int)])(p: ((Int, Int)) => Boolean): Option[(Int, Int)] = {
      val a = l.filter(p)

      if (a.size == 0)
        None

      else {
        val sum = a.foldRight((0, 0))((x: (Int, Int), y: (Int, Int)) => (x._1 + y._1, x._2 + y._2))
        Some((sum._1 / a.size, sum._2 / a.size))
      }
    }

    def minority(x: (Int, Int)): Boolean = x._1 < x._2
    def mayority(x: (Int, Int)): Boolean = x._1 > x._2
    def equallity(x: (Int, Int)): Boolean = x._1 == x._2
  }


  val colonyInfos: HashMap[Int, ColonyInfo] = {
    import sim.app.antDefenseAIs.model.TribeIDGenerator.nextTribeID

    var m = HashMap[Int, ColonyInfo]()

    for (i <- 0 until experiment.numberOfTribes) {
      val id = nextTribeID()

      val startPos = startPositions(i)
      val queen = new AntQueen(id, this, tribeTypes(i)) // generate queen
      assert(ants setObjectLocation (queen, startPos._1, startPos._2)) // Place queen on world

      val homePheromones = new DoubleGrid2D(width, height, 0.0d)
      homePheromones set (startPos._1, startPos._2, 1.0d) // Adapt home-pheromone on queens place

      val resPheromones = new DoubleGrid2D(width, height, 0.0d)
      val warPheromones = new DoubleGrid2D(width, height, 0.0d)

      m = m + ((id, new ColonyInfo(
        startPos, queen, homePheromones,
        resPheromones, warPheromones,
        hasKilled = 0, killed = 0, overaged = 0)))
    }

    m
  }

  /**
   * Starts the simulation of the world
   */
  def start() {
    experiment.schedule.scheduleRepeating(this)

    for ((id, cInfo) <- colonyInfos) { // Schedule all queens
      val queen = cInfo.queen
      val stoper = experiment.schedule.scheduleRepeating(queen)
      stopOrders += ((queen, stoper))
    }
  }


  //--------------------------- Nature behaviour ----------------------------------------

  // Stoppable object for each ant to take it out of scheduling
  private val stopOrders: mutable.HashMap[Ant, Stoppable] = mutable.HashMap()

  override def step(state: SimState) {


    // Remove deceased (i.e. too old) ants
    for (ant <- allAnts) {
      ant match {
        case _ if ant.isOveraged() || ant.isKilled() => removeAnt(ant)   // TODO: Decide to do it here or in hit()
        case other: Ant => other.mature()
      }
    }

    // Evaporation of war-pheromones
    for ((id, cInfo) <- colonyInfos) {
      val warPheroMap = cInfo.warPheromones

      for (i <- 0 until width; j <- 0 until height) {
        val threshold = 0.1e-2 // lowest possible value

        val old = warPheroMap.get(i, j)
        val evaporationRate: Double = 0.9

        val newer = if (old < threshold) 0 else old * evaporationRate
        warPheroMap set(i, j, newer)
      }
    }

    // Other evaporation can be implemented here

    // Diffusion can be implemented here
  }

  /**
   * Removes an ant cleanly from this world
   *
   * @param ant Ant to remove
   */
  private def removeAnt(ant: Ant) {
    if (!ant.isKilled() && !ant.isOveraged()) // If no reason for removing found
      throw new IllegalStateException("No reason found")

    else if (ant.isKilled())     // Adapt statistic of tribe in function of the reason
      colonyInfos(ant.tribeID).killed += 1
    else // ant.isOveraged
      colonyInfos(ant.tribeID).overaged += 1

    ant match {  // Drop wore resources
      case worker: AntWorker => worker.dropResources()
      case queen: AntQueen => queen.dropDeposit()
    }

    stopOrders.get(ant).get.stop() // Take ant out of scheduling
    stopOrders.-=(ant)
    ants.remove(ant)               // Remove ant from map
  }


  ////////////////////////// Operations on the map ///////////////////////////////////

  /**
   * Amount of resources on the given position
   *
   * @param pos Position of which the intensity of resources should be counted
   * @return Amount of resources on the given position
   */
  def resOn(pos: (Int, Int)): Int = resources.get(pos._1, pos._2)

  /**
   * Sets a new intensity of resources on the given position
   *
   * @param pos Position of which the intensity of resources should be set
   * @param amount New intensity of resources on the given position
   */
  def setResOn(pos: (Int, Int), amount: Int) {
    resources.set(pos._1, pos._2, amount)
  }

  /**
   * Ants on the given location
   *
   * @param position Position of which the returned ants are
   * @return Ants on the given location
   */
  def antsOn(position: (Int, Int)): List[Ant] = {
    val bag = ants.getObjectsAtLocation(toInd2D(position))

    if (bag == null)
      List()
    else
      antBag2AntList(bag)
  }

  /**
   * Moves the given ant into the given direction.
   *
   * @param ant Ant to move
   * @param direction Direction to move the ant to
   */
  private[model] def move(ant: Ant, direction: Direction.Value) {
    val targetPosition = Direction.inDirection(currentPosOf(ant).get, direction) // ant must live

    if (antsOn(targetPosition).size >= maxAntsPerField)
      throw new IllegalStateException("move: Target position is already full")
    else
      ants.setObjectLocation(ant, toInd2D(targetPosition))
  }

  /**
   * Home pheromone intensity of the tribe of the given ant in the given direction
   *
   * @param ant Ant which wants the result
   * @param dir Direction where to investigate the pheromone intensity
   * @return Home pheromone intensity of the tribe of the given ant in the given position
   */
  private[model] def homePheroOf(ant: Ant, dir: Direction.Value): Option[Double] =
    currentPosOf(ant) flatMap(pos => {
      val pheroPos = Direction.inDirection(pos, dir)
      Some(colonyInfos(ant.tribeID).homePheromones get (pheroPos._1, pheroPos._2))
    })

  /**
   * Home pheromone intensity of the tribe of the given ant at its current position
   *
   * @param ant Ant which wants the result
   * @return Home pheromone intensity of the tribe of the given at its current position
   */
  private[model] def homePheroOf(ant: Ant): Option[Double] =
    currentPosOf(ant) flatMap (pos => Some(colonyInfos(ant.tribeID).homePheromones.get(pos._1, pos._2)))

  /**
   * Resource pheromone intensity of the tribe of the given ant in the given direction
   *
   * @param ant Ant which wants the result
   * @param dir Direction where to investigate the pheromone intensity
   * @return Resource pheromone intensity of the tribe of the given ant in the given position
   */
  private[model] def resPheroOf(ant: Ant, dir: Direction.Value): Option[Double] =
    currentPosOf(ant) flatMap(pos => {
      val pheroPos = Direction.inDirection(pos, dir)
      Some(colonyInfos(ant.tribeID).resPheromones.get(pheroPos._1, pheroPos._2))
    })

  /**
   * Resource pheromone intensity of the tribe of the given ant at its current position
   *
   * @param ant Ant which wants the result
   * @return Resource pheromone intensity of the tribe of the given at its current position
   */
  private[model] def resPheroOf(ant: Ant): Option[Double] =
    currentPosOf(ant) flatMap (pos => Some(colonyInfos(ant.tribeID).resPheromones.get(pos._1, pos._2)))

  /**
   * War pheromone intensity of the tribe of the given ant in the given direction
   *
   * @param ant Ant which wants the result
   * @param dir Direction where to investigate the pheromone intensity
   * @return War pheromone intensity of the tribe of the given ant in the given position
   */
  private[model] def warPheroOf(ant: Ant, dir: Direction.Value): Option[Double] =
    currentPosOf(ant) flatMap (pos => {
      val pheroPos = Direction.inDirection(pos, dir)
      Some(colonyInfos(ant.tribeID).warPheromones.get(pheroPos._1, pheroPos._2))
    })

  /**
   * War pheromone intensity of the tribe of the given ant at its current position
   *
   * @param ant Ant which wants the result
   * @return War pheromone intensity of the tribe of the given at its current position
   */
  private[model] def warPheroOf(ant: Ant): Option[Double] =
    currentPosOf(ant) flatMap (pos => Some(colonyInfos(ant.tribeID).warPheromones.get(pos._1, pos._2)))

  /**
   * Set home pheromone intensity of the tribe of the given ant at the given position
   *
   * @param ant Ant which wants to set the pheromone intensity of its tribe
   * @param pos Position where to set the pheromone intensity
   * @param amount New intensity
   */
  private[model] def setHomePheroOn(ant: Ant, pos: (Int, Int), amount: Double) {
    colonyInfos(ant.tribeID).homePheromones.set(pos._1, pos._2, amount)
  }

  /**
   * Set resource pheromone intensity of the tribe of the given ant at the given position
   *
   * @param ant Ant which wants to set the pheromone intensity of its tribe
   * @param pos Position where to set the pheromone intensity
   * @param amount New intensity
   */
  private[model] def setResPheroOn(ant: Ant, pos: (Int, Int), amount: Double) {
    colonyInfos(ant.tribeID).resPheromones.set(pos._1, pos._2, amount)
  }

  /**
   * Set resource pheromone intensity of the tribe of the given ant at the given position
   *
   * @param ant Ant which wants to set the pheromone intensity of its tribe
   * @param pos Position where to set the pheromone intensity
   * @param amount New intensity
   */
  private[model] def setWarPheroOn(ant: Ant, pos: (Int, Int), amount: Double) {
    colonyInfos(ant.tribeID).warPheromones set (pos._1, pos._2, amount)
  }

  /**
   * Give a hit from one ant to the other
   *
   * Internals of ant behaviour routed via World-class to collect statistical data.
   *
   * @param giver Ant hitting an opponent
   * @param receiver Ant being hit
   */
  private[model] def hit(giver: Ant)(receiver: Ant) {
    val nearAttackingAnts = antsInNeighbourhoodOf(currentPosOf(giver).get).count(a => a.tribeID == giver.tribeID)
    val nearAttackedAnts = antsInNeighbourhoodOf(currentPosOf(receiver).get).count(a => a.tribeID == receiver.tribeID)

    val cInfoAttacker = colonyInfos(giver.tribeID)
    val cInfoAttacked = colonyInfos(receiver.tribeID)

    val aList = cInfoAttacker._hit_made.::((nearAttackingAnts, nearAttackedAnts))
    cInfoAttacker._hit_made = aList

    val dList = cInfoAttacked._hit_gotten.::((nearAttackedAnts, nearAttackingAnts))
    cInfoAttacked._hit_gotten = dList

    receiver.receiveHitFrom(giver)
//    if (receiver.isKilled())  TODO: decide between this variant and the other to remove them by world scheduling
//      removeAnt(receiver)
  }


  //--------------------------- Other methods for the ants -------------------------------

  /**
   * Calculates all the neighbour positions within a certain distance.
   * Current position is not included.
   *
   * @param distance Maximum distance of a field towards the current position of the ant. Default is `1`
   * @return List of positions within the given range.
   */
  def nearPos(ant: Ant, distance: Int = 1): Option[List[(Int, Int)]] =
    neighbourhood(ant, distance) flatMap (poss => Some(poss.filter(pos => pos != currentPosOf(ant).get)))


  /**
   * Calculates all the neighbour positions within a certain distance.
   *
   * @param distance Maximum distance of a field towards the current position of the ant. Default is `1`
   * @return List of positions within the given range.
   */
  def neighbourhood(ant: Ant, distance: Int = 1): Option[List[(Int, Int)]] =
    currentPosOf(ant) flatMap (pos => {
      val (xBag, yBag) = neighbourhoodBags (pos, distance)
      Some(toTupleList(xBag, yBag))
    })

  /**
   * Calculates in two bags the x-positions an the y-positions of the neighbourhood within a given range
   *
   * @param distance Maximum distance of a field towards the current position of the ant
   * @return Tuple of bags with the x and the corresponding y-positions
   */
  private[this] def neighbourhoodBags(pos: (Int, Int), distance: Int): (IntBag, IntBag) = {
    val xBag: IntBag = new IntBag()
    val yBag: IntBag = new IntBag()
    ants.getNeighborsMaxDistance(pos._1, pos._2, distance, false, xBag, yBag)
    (xBag, yBag)
  }

  /**
   * All directions in which a given ant can move from its current position
   *
   * @param ant Ant for which all possible movement directions are calculated
   * @return All directions in which an given ant can move from its current position
   */
  private[model] def validDirections(ant: Ant): Option[List[Direction.Value]] = {
    val pos = currentPosOf(ant)

    if (pos.isEmpty)
      None

    else {
      // True iff target position in that direction exists
      def isValid(dir: Direction.Value): Boolean = {
        val neighbours = neighbourhood(ant).get
        val destiny = Direction.inDirection(pos.get, dir)
        neighbours.contains(destiny)
      }

      // True iff not too many ants already there
      def spaceLeft(dir: Direction.Value): Boolean = {
        val destiny = Direction.inDirection(pos.get, dir)
        antsOn(destiny).size < maxAntsPerField
      }

      // Filtering order might be important - otherwise it could occur to examine a non existing field
      Some(Direction.values.filter(isValid).filter(spaceLeft).toList)
    }
  }


  /**
   * Returns the current position of the asking ant
   *
   * @param ant Ant asking for her current position
   * @return Current position of `ant`.
   */
  def currentPosOf(ant: Ant): Option[(Int, Int)] = {
    ants.getObjectLocation(ant) match {
      case null => None
      case location => Some(toTuple(location))
    }
  }

  /**
   * Returns a reference to the queen of the given ant
   *
   * @param ant Ant asking for the queen
   * @return Queen of the ant colony the ant belongs to
   */
  private[model] def queenOf(ant: Ant): AntQueen = colonyInfos(ant.tribeID).queen

  private[model] def allAnts: List[Ant] = antBag2AntList (ants.getAllObjects)

  /**
   * All ants in the range-neighbourhood of a given position
   *
   * @param range Range of the neighbourhood
   * @param pos Central position of the neighbourhood
   * @return List of all ants in the given neighbourhood
   */
  private[model] def antsInNeighbourhoodOf(pos: (Int, Int), range: Int = 1): List[Ant] = {
    val (xBag, yBag) = neighbourhoodBags(pos, range)
    val bag = new Bag()
    ants.getObjectsAtLocations(xBag, yBag, bag)
    antBag2AntList(bag)
  }

  /**
   * Places a given, new ant on the given position
   *
   * @param ant Ant to place on the map
   * @param pos Position to place the given ant on
   */
 private[this] def placeNewAnt(ant: Ant, pos: (Int, Int)) {
   if (ants.getObjectLocation(ant) != null)
     throw new IllegalStateException("Ant already placed on world")
   if (colonyInfos(ant.tribeID).population() >= maxPopulation)
     throw new IllegalStateException("Maximum population of " + maxPopulation + " already reached" +
      " by tribe " + ant.tribeID)

   assert(ants.setObjectLocation(ant, toInd2D(pos)))
   val stoper = experiment.schedule.scheduleRepeating(ant)
   stopOrders += ((ant, stoper))
 }

  /**
   * Places a given, new ant on the same field of its colony queen
   *
   * @param ant Ant to place on the map
   */
 private[model] def placeNewAnt(ant: Ant) {
   placeNewAnt(ant, currentPosOf(queenOf(ant)).get) // queen must have invoked this method
 }


  //-------------------------- Others --------------------------------------

  /**
   * All tribe ids of the tribes on the world
   *
   * @return Array of all tribe ids of the tribes on the world
   */
  def tribeIDs(): Array[Int] = colonyInfos.keys.toArray

  /**
   * Returns map containing the current resource distribution
   *
   * @return Current resource distribution
   */
  def resourceMap(): Array[Array[Int]] = {
    val result = Array.ofDim[Int](width, height)
    for (i <- 0 until result.length; j <- 0 until result(i).length)
      result(i)(j) = resources.get(i, j)

    result
  }
}
