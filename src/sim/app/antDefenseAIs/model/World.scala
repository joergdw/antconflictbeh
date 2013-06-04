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
import sim.util.IntBag

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
 */
private[antDefenseAIs] final class World(
  val experiment: Experiment,
  val width: Int,
  val height: Int,
  val maxPopulation: Int = Int.MaxValue,
  private val startPositions: Array[(Int, Int)],
  val resources: IntGrid2D,
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

    val NorthWest = Value(0, "north west") /** Direction north west */
    val North = Value(1, "north") /** Direction north */
    val NorthEast = Value(2, "north east") /** Direction north east */
    val East = Value(3, "east") /** Direction east */
    val SouthEast = Value(4, "south east") /** Direction south west */
    val South = Value(5, "south") /** Direction south */
    val SouthWest = Value(6, "south west") /** Direction south-west */
    val West = Value(7, "west") /** Direction west */

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
      (NorthWest, (-1, -1)),
      (North, (0, -1)),
      (NorthEast, (1, -1)),
      (West, (-1, 0)),
      (East, (1, 0)),
      (SouthWest, (-1, 1)),
      (South, (0, 1)),
      (SouthEast, (1, 1))
    )

    private val assocsm1 = HashMap( // assocs inverse
      ((-1, -1), NorthWest),
      ((0, -1), North),
      ((1, -1), NorthEast),
      ((-1, 0), West),
      ((1, 0), East),
      ((-1, 1), SouthWest),
      ((0, 1), South),
      ((1, 1), SouthEast)
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

  private val queens: HashMap[Int, AntQueen] = {
    import sim.app.antDefenseAIs.model.TribeIDGenerator.nextTribeID

    var m = HashMap[Int, AntQueen]()

    for (i <- 0 until experiment.numberOfTribes) {
      val id = nextTribeID()
      val queen = new AntQueen(id, this, tribeTypes(i)) // generate queen
      assert(ants.setObjectLocation(queen, startPositions(i)._1, startPositions(i)._2)) // Place queen on world

      m = m + ((id, queen))
    }

    m
  }

  /**
   * Map of all start positions for each tribe
   */
  val startPositionsByID: HashMap[Int, (Int, Int)] = {
    var m = HashMap[Int, (Int, Int)]()

    for ((id, queen) <- queens)
      m = m + ((id, currentPos(queen).get))

    m
  }

  // For each tribe there will be a store for all the pheromone-types.
  // Only public for mason graphical capabilities. Other classes should use the access methods of this class
  val homePheromones: HashMap[Int, DoubleGrid2D] = {
    var m: HashMap[Int, DoubleGrid2D] = HashMap()

    for (id <- queens.keys)
      m = m + ((id, new DoubleGrid2D(width, height, 0.0d)))

    m
  }

  val resPheromones: HashMap[Int, DoubleGrid2D] = {
    var m: HashMap[Int, DoubleGrid2D] = HashMap()

    for (id <- queens.keys)
      m = m + ((id, new DoubleGrid2D(width, height, 0.0d)))

    m
  }

  val warPheromones: HashMap[Int, DoubleGrid2D] = {
    var m: HashMap[Int, DoubleGrid2D] = HashMap()

    for (id <- queens.keys)
      m = m + ((id, new DoubleGrid2D(width, height, 0.0d)))

    m
  }

  // Adapt home-pheromone on queens place
  for ((id, queen) <- queens) {
    val homePheroMap = homePheromones(id)
    val pos = currentPos(queen).get
    homePheroMap.set(pos._1, pos._2, 1.0d)
  }

  /**
   * Starts the simulation of the world
   */
  def start() {
    experiment.schedule.scheduleRepeating(this)

    for ((id, queen) <- queens) { // Schedule all queens
      val stoper = experiment.schedule.scheduleRepeating(queen)
      stopOrders += ((queen, stoper))
    }
  }


  /////////////////////////// Nature behaviour ///////////////////////////////////////

  // Stoppable object for each ant to take it out of scheduling
  private val stopOrders: mutable.HashMap[Ant, Stoppable] = mutable.HashMap()

  override def step(state: SimState) {
    def removeAnt(ant: Ant) { // Actions to do to remove an ant
      ant match {
        case worker: AntWorker => worker.dropResources()
        case queen: AntQueen => queen.dropDeposit()
      }

      stopOrders.get(ant).get.stop() // Take ant out of scheduling
      stopOrders.-=(ant)
      ants.remove(ant)               // Remove ant from map
    }

    // Remove dead ants and too old ants from the world and age and schedule again all other ants
    for (ant <- allAnts) {
      ant match {
        case _ if ant.isKilled => {
          removeAnt(ant)
          val n = _killedAntsByTribe(ant.tribeID)
          _killedAntsByTribe += ((ant.tribeID, n + 1))
        }
        case _ if ant.age >= ant.maximumAge => {
          removeAnt(ant)
          val n = _diedAntsByTribe(ant.tribeID)
          _diedAntsByTribe += ((ant.tribeID, n + 1))
        }
        case other: Ant => other.age += 1  // age a living ant
      }
    }

    // Evaporation
    for ((id, warPheroMap) <- warPheromones) {
      for (i <- 0 until width; j <- 0 until height) {
        val threshold = 0.1e-2 // lowest possible value

        val old = warPheroMap.get(i, j)
        val evaporationRate: Double = 0.9

        val newer = if (old < threshold) 0 else old * evaporationRate
        warPheroMap.set(i, j, newer)
      }
    }

    // Evaporation for war-pheromones
    for (warPheroMap <- warPheromones.values) {
      for (i <- 0 until height; j <- 0 until width) {
        val threshold = 0.1e-40 // lowest possible value

        val old = warPheroMap.get(i, j)
        val evaporationRate: Double = 0.98

        val newer = if (old < threshold) 0 else old * evaporationRate
        warPheroMap.set(i, j, newer)
      }
    }

    // Other evaporation can be implemented here

    // Diffusion can be implemented here
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
    val targetPosition = Direction.inDirection(currentPos(ant).get, direction) // ant must live
    ants.setObjectLocation(ant, toInd2D(targetPosition))
  }

  /**
   * Home pheromone intensity of the tribe of the given ant in the given direction
   *
   * @param ant Ant which wants the result
   * @param dir Direction where to investigate the pheromone intensity
   * @return Home pheromone intensity of the tribe of the given ant in the given position
   */
  private[model] def homePheroOf(ant: Ant, dir: Direction.Value): Option[Double] = {
    currentPos(ant) match {
      case None => None
      case Some(pos) => {
        val pheroPos = Direction.inDirection(pos, dir)
        Some(homePheromones(ant.tribeID).get(pheroPos._1, pheroPos._2))
      }
    }
  }

  /**
   * Home pheromone intensity of the tribe of the given ant at its current position
   *
   * @param ant Ant which wants the result
   * @return Home pheromone intensity of the tribe of the given at its current position
   */
  private[model] def homePheroOf(ant: Ant): Option[Double] = {
    currentPos(ant) match {
      case None => None
      case Some(pos) => Some(homePheromones(ant.tribeID).get(pos._1, pos._2))
    }
  }

  /**
   * Resource pheromone intensity of the tribe of the given ant in the given direction
   *
   * @param ant Ant which wants the result
   * @param dir Direction where to investigate the pheromone intensity
   * @return Resource pheromone intensity of the tribe of the given ant in the given position
   */
  private[model] def resPheroOf(ant: Ant, dir: Direction.Value): Option[Double] = {
    currentPos(ant) match {
      case None => None
      case Some(pos) => {
        val pheroPos = Direction.inDirection(pos, dir)
        Some(resPheromones(ant.tribeID).get(pheroPos._1, pheroPos._2))
      }
    }
  }

  /**
   * Resource pheromone intensity of the tribe of the given ant at its current position
   *
   * @param ant Ant which wants the result
   * @return Resource pheromone intensity of the tribe of the given at its current position
   */
  private[model] def resPheroOf(ant: Ant): Option[Double] = {
    currentPos(ant) match {
      case None => None
      case Some(pos) => Some(resPheromones(ant.tribeID).get(pos._1, pos._2))
    }
  }

  /**
   * War pheromone intensity of the tribe of the given ant in the given direction
   *
   * @param ant Ant which wants the result
   * @param dir Direction where to investigate the pheromone intensity
   * @return War pheromone intensity of the tribe of the given ant in the given position
   */
  private[model] def warPheroOf(ant: Ant, dir: Direction.Value): Option[Double] = {
    currentPos(ant) match {
      case None => None
      case Some(pos) => {
        val pheroPos = Direction.inDirection(pos, dir)
        Some(warPheromones(ant.tribeID).get(pheroPos._1, pheroPos._2))
      }
    }
  }

  /**
   * War pheromone intensity of the tribe of the given ant at its current position
   *
   * @param ant Ant which wants the result
   * @return War pheromone intensity of the tribe of the given at its current position
   */
  private[model] def warPheroOf(ant: Ant): Option[Double] = {
    currentPos(ant) match {
      case None => None
      case Some(pos) => Some(warPheromones(ant.tribeID).get(pos._1, pos._2))
    }
  }

  /**
   * Set home pheromone intensity of the tribe of the given ant at the given position
   *
   * @param ant Ant which wants to set the pheromone intensity of its tribe
   * @param pos Position where to set the pheromone intensity
   * @param amount New intensity
   */
  private[model] def setHomePheroOn(ant: Ant, pos: (Int, Int), amount: Double) {
    homePheromones(ant.tribeID).set(pos._1, pos._2, amount)
  }

  /**
   * Set resource pheromone intensity of the tribe of the given ant at the given position
   *
   * @param ant Ant which wants to set the pheromone intensity of its tribe
   * @param pos Position where to set the pheromone intensity
   * @param amount New intensity
   */
  private[model] def setResPheroOn(ant: Ant, pos: (Int, Int), amount: Double) {
    resPheromones(ant.tribeID).set(pos._1, pos._2, amount)
  }

  /**
   * Set resource pheromone intensity of the tribe of the given ant at the given position
   *
   * @param ant Ant which wants to set the pheromone intensity of its tribe
   * @param pos Position where to set the pheromone intensity
   * @param amount New intensity
   */
  private[model] def setWarPheroOn(ant: Ant, pos: (Int, Int), amount: Double) {
    warPheromones(ant.tribeID).set(pos._1, pos._2, amount)
  }


  ///////////////////////// Other methods for the ants ///////////////////////////////

  /**
   * Calculates all the neighbour positions within a certain distance.
   * Current position is not included.
   *
   * @param distance Maximum distance of a field towards the current position of the ant. Default is `1`
   * @return List of positions within the given range.
   */
  def nearPos(ant: Ant, distance: Int = 1): Option[List[(Int, Int)]] =
    neighbourhood(ant, distance) match {
      case None => None
      case Some(poss) => Some(poss.filter(pos => pos != currentPos(ant).get))
    }


  /**
   * Calculates all the neighbour positions within a certain distance.
   *
   * @param distance Maximum distance of a field towards the current position of the ant. Default is `1`
   * @return List of positions within the given range.
   */
  def neighbourhood(ant: Ant, distance: Int = 1): Option[List[(Int, Int)]] = {
    currentPos(ant) match {
      case None => None
      case Some(pos) => {
        val (xBag, yBag) = neighbourhoodBags(pos, distance)
        Some(toTupleList(xBag, yBag))
      }
    }
  }

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
    val pos = currentPos(ant)

    if (pos.isEmpty)
      None

    else {
      def isValid(dir: Direction.Value): Boolean = {
        val neighbours = neighbourhood(ant).get
        val destiny = Direction.inDirection(pos.get, dir)
        neighbours.contains(destiny)
      }

      Some(Direction.values.filter(isValid).toList)
    }
  }


  /**
   * Returns the current position of the asking ant
   *
   * @param ant Ant asking for her current position
   * @return Current position of `ant`.
   */
  def currentPos(ant: Ant): Option[(Int, Int)] = {
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
    private[model] def queenOf(ant: Ant): AntQueen = queens(ant.tribeID)

  /**
   * Places a given, new ant on the given position
   *
   * @param ant Ant to place on the map
   * @param pos Position to place the given ant on
   */
 private[this] def placeNewAnt(ant: Ant, pos: (Int, Int)) {
   if (ants.getObjectLocation(ant) != null)
     throw new IllegalStateException("Ant already placed on world")
   if (populationStat()(ant.tribeID) >= maxPopulation)
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
   placeNewAnt(ant, currentPos(queenOf(ant)).get) // queen must have invoked this method
 }



  ///////////////////////// Statistic related stuff /////////////////////////////////

  /** Number of ants killed by foreign colonies by each tribe */
  private[this] val _killedAntsByTribe: mutable.HashMap[Int, Int] = {
    val result = mutable.HashMap[Int, Int]()

    for (queen <- queens.values) {
      result += ((queen.tribeID, 0))
    }

    result
  }

  /** Number of ants died because of age by each tribe */
  private[this] val _diedAntsByTribe: mutable.HashMap[Int, Int] = {
    val result = mutable.HashMap[Int, Int]()

    for (queen <- queens.values) {
      result += ((queen.tribeID, 0))
    }

    result
  }

  /**
   * Total number of ants lost by each tribe
   *
   * @return Total number of ants lost by each tribe
   */
  def lostAntsByTribe(): HashMap[Int, Int] = {
    var result = HashMap[Int, Int]()

    for (id <- queens.keys)
      result = result + ((id, _killedAntsByTribe(id) + _diedAntsByTribe(id)))

    result
  }

  /**
   * Lost ants by each tribe due to overaging
   *
   * @return Lost ants by each tribe due to overaging
   */
  def lostAntsByAge(): mutable.HashMap[Int, Int] = _diedAntsByTribe.clone()

  /**
   * Counts population of all tribes
   *
   * @return HashMap that contains for each tribeID (key) the corresponding population
   */
  def populationStat(): HashMap[Int, Int] = {
    val objects = ants.getAllObjects
    var result = HashMap[Int, Int]()

    for (queen <- queens.values)
      result = result + ((queen.tribeID, 0)) // Start with 0 for each tribe

    for (i <- 0 until objects.size()) {
      val ant = objects.get(i).asInstanceOf[Ant]
      val n: Int = result(ant.tribeID)
      result = result + ((ant.tribeID, n + 1))
    }

    result
  }

  /**
   * Counts resources owned by the ant queen of each tribe
   *
   * @return field i contains the amount of resources the queen of tribe i has
   */
  def resourceStat(): HashMap[Int, Int] = {
    var result = HashMap[Int, Int]()

    for (queen <- queens.values)
      result = result + ((queen.tribeID, queen.deposit))

    result
  }

  /**
   * Amount of resources owned at the current time by each tribe.
   *
   * @return Field i contains the total intensity of resources owned by ants of the tribe i
   */
  def totalResStat(): mutable.HashMap[Int, Int] = {
    val ants: List[Ant]  = allAnts
    val result = mutable.HashMap[Int, Int]()

    for (queen <- queens.values)
      result += ((queen.tribeID, 0)) // Start with 0 for each tribe

    for (ant <- ants) {
      val n: Int = result(ant.tribeID)
      val diff: Int = ant match {
                        case worker: AntWorker => worker.inBackpack
                        case queen: AntQueen => queen.deposit
                      }
      result += ((ant.tribeID, n + diff))
    }

    result.clone()
  }

  def queensSurvived(): HashMap[Int, Boolean] = {
    var result = HashMap[Int, Boolean]()
    for ((id, queen) <- queens) {
      result = result + ((id, ants.getObjectLocation(queen) != null))
    }

    result
  }


  ///////////////////////////// Others ///////////////////////////////////

  /**
   * All tribe ids of the tribes on the world
   *
   * @return Array of all tribe ids of the tribes on the world
   */
  def tribeIDs(): Array[Int] = queens.keys.toArray

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

  private def allAnts: List[Ant] = antBag2AntList (ants.getAllObjects)
}
