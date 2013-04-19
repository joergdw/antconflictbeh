/*
 * Copyright © 2013 by Jörg D. Weisbarth <joerg.bretten@web.de>
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
package AntDefenseAIs.Model

import sim.field.grid.{DoubleGrid2D, IntGrid2D, SparseGrid2D}
import sim.util.IntBag
import sim.engine.{SimState, Steppable}

import AntDefenseAIs.Common.Common._
import TribeIDGenerator.nextTribeID

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
 * @param sim Simulation the world is related to
 * @param resources Distribution of the resources on the map
 * @param height Height of the map
 * @param width  Width of the map
 * @param tribeTypes Constructors of the different types of the tribe
 */
final class World(
  val sim: Simulation,
  val height: Int, val width: Int,
  private val startPositions: Array[(Int, Int)],
  val resources: IntGrid2D,
  private val tribeTypes: Array[AntGenerator]) extends Steppable {

  val gamma: Double = 0.9d /** Learning parameter according the one used paper */
  val explorationRate: Double = 0.2d
  val maxResAmount: Int = 10 /** Maximum number of resources on a field */
  val pheroThreshould: Double = 0.0000000001d /** Next phero-value: zero */

  val maxPopulation: Int = Int.MaxValue /** Maximum tribe population */
  val startRessources: Int = 20 /** amount of res a tribe starts with */
  val productionTime: Int = 10 /** time to produce an ant*/
  val productionCost: Int = 1 /** costs to produce an ant */

  val random = sim.random /** Random numbergenerator */

  if (startPositions.length != tribeTypes.length)
    throw new IllegalArgumentException("Not exactly as many start positions as colony types")

  // ASSERT: all start positions in range of `height` and `weight`

  val ants: SparseGrid2D = new SparseGrid2D(height, width) /** Agents: multiples can be on one field */

  // For each tribe there will be a store for all the pheromone-types.
  // Only public for mason graphical capabilities. Other classes should use the access methods of this class
  val homePheromones = new Array[IntGrid2D](tribeTypes.length)
  val resPheromones = new Array[DoubleGrid2D](tribeTypes.length)
  val warPheromones = new Array[DoubleGrid2D](tribeTypes.length)

  private val queens: Array[AntQueen] = new Array[AntQueen](tribeTypes.length)

  for (i <- 0 until tribeTypes.length) { // foreach tribe
    // Create pheromone maps
    homePheromones(i) = new IntGrid2D(height, width, Integer.MAX_VALUE)
    resPheromones(i) = new DoubleGrid2D(height, width, 0.0d)
    warPheromones(i) = new DoubleGrid2D(height, width, 0.0d)

    homePheromones(i).set(startPositions(i)._1, startPositions(i)._2, 0) // Adapt start position of the queen

    queens(i) = new AntQueen(nextTribeID(), this, tribeTypes(i)) // Create queen
  }

  /**
   * Starts the simulation of the world
   */
  def start() {
    sim.schedule.scheduleRepeating(this)

    for (queen <- queens)
      sim.schedule.scheduleRepeating(queen)
  }

//  /**
//   * Sets a resource distribution.
//   *
//   * This can only be executed one time.
//   *
//   * @param r Resourcedistribution
//   */
//  private[Simulation] def setResourceMap(r: IntGrid2D) {
//    if (!distributionSetable)
//      throw new IllegalStateException("Resource distribution can only be set one time.")
//    else if (r.getWidth != width || r.getHeight != height)
//      throw new IllegalArgumentException("Size does not accord with world size")
//    else {
//      resources = r
//      distributionSetable = false
//    }
//  }


  /////////////////////////// Nature behaviour ///////////////////////////////////////

  def step(state: SimState) {

    // Remove dead ants from the map
    for (ant <- allAnts) {
      ant match {
        case worker: AntWorker => if (worker.isDead) {
          worker.dropResources(); ants.remove(worker) // Take ant out of scheduling
        }
      }
    }

    // Evaporation can be implemented here
    // Diffusion can be implemented here
  }


  ////////////////////////// Operations on the map ///////////////////////////////////

  /**
   * Amount of resources on the given position
   *
   * @param pos Position of which the amount of resources should be counted
   * @return Amount of resources on the given position
   */
  def resOn(pos: (Int, Int)): Int = resources.get(pos._1, pos._2)

  /**
   * Sets a new amount of resources on the given position
   *
   * @param pos Position of which the amount of resources should be set
   * @param amount New amount of resources on the given position
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

    // Iterate through all objects on that field.
    val bag = ants.getObjectsAtLocation(position)

    val result: IndexedSeq[Ant] = for (i: Int <- 0 until bag.size()) yield
      bag.get(i).asInstanceOf[Ant]

    result.toList
  }

  /**
   * Moves the given ant to the given position. Position must be a neighbour position of the ant.
   *
   * @param ant Ant to move
   * @param newPos Position to move the ant to
   */
  private[Model] def moveTo(ant: Ant, newPos: (Int, Int)) {
    if (nearPos(ant) contains newPos)
      ants.setObjectLocation(ant, toInd2D(newPos))
    else
      throw new IllegalArgumentException("Destiny is not a neighbour field")
  }

  /**
   * Home pheromone intensity of the tribe of the given ant at the given position
   *
   * @param ant Ant which wants the result
   * @param pos Position where to investigate the pheromone intensity
   * @return Home pheromone intensity of the tribe of the given ant at the given position
   */
  private[Model] def homePheroOn(ant: Ant, pos: (Int, Int)): Int = homePheromones(ant.tribeID).get(pos._1, pos._2)

  /**
   * Resource pheromone intensity of the tribe of the given ant at the given position
   *
   * @param ant Ant which wants the result
   * @param pos Position where to investigate the pheromone intensity
   * @return Resource pheromone intensity of the tribe of the given ant at the given position
   */
  private[Model] def resPheroOn(ant: Ant, pos: (Int, Int)): Double = resPheromones(ant.tribeID).get(pos._1, pos._2)

  /**
   * War pheromone map of the tribe of the given ant
   *
   * @param ant Ant which wants the result
   * @param pos Position where to investigate the pheromone intensity
   * @return War pheromone map of the tribe of the given ant
   */
  private[Model] def warPheroOn(ant: Ant, pos: (Int, Int)): Double = warPheromones(ant.tribeID).get(pos._1, pos._2)

  /**
   * Set home pheromone intensity of the tribe of the given ant at the given position
   *
   * @param ant Ant which wants to set the pheromone intensity of its tribe
   * @param pos Position where to set the pheromone intensity
   * @param amount New intensity
   */
  private[Model] def setHomePheroOn(ant: Ant, pos: (Int, Int), amount: Int) {
    homePheromones(ant.tribeID).set(pos._1, pos._2, amount)
  }

  /**
   * Set resource pheromone intensity of the tribe of the given ant at the given position
   *
   * @param ant Ant which wants to set the pheromone intensity of its tribe
   * @param pos Position where to set the pheromone intensity
   * @param amount New intensity
   */
  private[Model] def setResPheroOn(ant: Ant, pos: (Int, Int), amount: Double) {
    resPheromones(ant.tribeID).set(pos._1, pos._2, amount)
  }

  /**
   * Set resource pheromone intensity of the tribe of the given ant at the given position
   *
   * @param ant Ant which wants to set the pheromone intensity of its tribe
   * @param pos Position where to set the pheromone intensity
   * @param amount New intensity
   */
  private[Model] def setWarPheroOn(ant: Ant, pos: (Int, Int), amount: Double) {
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
  def nearPos(ant: Ant, distance: Int = 1): List[(Int, Int)] = {
    val (xBag, yBag) = nearPosBags(ant, distance)
    toTupleList(xBag, yBag) filterNot equals
  }

  /**
   * Calculates in two bags the x-positions an the y-positions of the neighbourhood within a given range
   *
   * @param distance Maximum distance of a field towards the current position of the ant
   * @return Tuple of bags with the x and the corresponding y-positions
   */
  private def nearPosBags(ant: Ant, distance: Int): (IntBag, IntBag) = {
    val (x, y) = currentPos(ant)
    val xBag: IntBag = new IntBag()
    val yBag: IntBag = new IntBag()
    ants.getNeighborsMaxDistance(x, y, distance, false, xBag, yBag)
    (xBag, yBag)
  }

  /**
   * Returns the current position of the asking ant
   *
   * @param ant Ant asking for her current position
   * @return Current position of `ant`.
   */
  def currentPos(ant: Ant): (Int, Int) = toTuple(ants.getObjectLocation(ant))

  /**
   * Returns a reference to the queen of the given ant
   *
   * @param ant Ant asking for the queen
   * @return Queen of the ant colony the ant belongs to
   */
 private[Model] def queenOf(ant: Ant): AntQueen = queens(ant.tribeID)

  /**
   * Places a given, new ant on the given position
   *
   * @param ant Ant to place on the map
   * @param pos Position to place the given ant on
   */
 private def placeNewAnt(ant: Ant, pos: (Int, Int)) {
   // ASSERT: Ant is new and not already placed on the world map.

   ants.setObjectLocation(ant, toInd2D(pos))
   sim.schedule scheduleRepeating(ant)
 }

  /**
   * Places a given, new ant on the same field of its colony queen
   *
   * @param ant Ant to place on the map
   */
 private[Model] def placeNewAnt(ant: Ant) {
   placeNewAnt(ant, queenOf(ant).currentPos)
 }



  ///////////////////////// Statistic related stuff /////////////////////////////////

  /**
   * Counts population of all tribes
   *
   * @return field i contains the total population of the tribe with the ID i
   */
  def populationStat: Array[Int] = {
    val objects = ants.getAllObjects
    val result = new Array[Int](tribeTypes.length)

    for (i <- 0 until objects.size()) {
      val ant = objects.get(i).asInstanceOf[Ant]
      result(ant.tribeID) += 1
    }

    result
  }

  /**
   * Counts resources owned by the ant queen of each tribe
   *
   * @return field i contains the amount of resources the queen of tribe i has
   */
  def resourceStat: Array[Int] = {
    val result = new Array[Int](queens.length)
    for (i <- 0 until result.length) {
      result(i) = queens(i).hasDeposit
    }

    result
  }

  /**
   * Amount of resources owned at the current time by each tribe.
   *
   * @return Field i contains the total amount of resources owned by ants of the tribe i
   */
  def totalResStat(): Array[Int] = {
    val ants: List[Ant]  = allAnts
    val result = new Array[Int](tribeTypes.length)

    for (ant <- ants) {
      ant match {
        case worker: AntWorker => result(worker.tribeID) += worker.inBackpack
        case queen: AntQueen => result(queen.tribeID) += queen.hasDeposit
        case otherAnt => throw new Exception("Counting rules for class " + otherAnt.getClass.getName + " not known")
      }
    }

    result
  }


  ///////////////////////////// Other AntDefenseAIs.Common ///////////////////////////////////

  /**
   * Returns map containing the current resource distribution
   *
   * @return Current resource distribution
   */
  def resourceMap(): Array[Array[Int]] = {
    val result = Array.ofDim[Int](height, height)
    for (i <- 0 until result.length; j <- 0 until result(i).length)
      result(i)(j) = resources.get(i, j)

    result
  }

  private def allAnts: List[Ant] = antBag2AntList (ants.getAllObjects)
}