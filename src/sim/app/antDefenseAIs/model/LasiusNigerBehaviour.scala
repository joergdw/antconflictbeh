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
package sim.app.antDefenseAIs.model


private[antDefenseAIs] object LasiusNigerBehaviour {
  val antsSensingRange: Int = 3 /** Radius of the area the ant can sense other individuals */
  val notBored: Int = 100 /** Value of boredom, 100 if an ant is not bored at all */
}

import StrictMath.min

import sim.engine.SimState

import LasiusNigerBehaviour._
import ec.util.MersenneTwisterFast

/**
 * Behaviour description of an ant worker
 *
 * @param antWorker ant worker equipped with this behaviour
 * @param emotionalDwellTime How long an individual stays in an individual state before going to another
 * @param maxAggressiveness As of this value of other ants of the same colony, the ant changes state with probability
 *                          `maxAggressivenessProb`.
 * @param maxAggressivenessProb Highest possible probability that an ant gets aggressive
 * @param minAggressivenessProb Lowest possible probability that an ant gets aggressive
 * @param alpha Influence of pheromone for determine next position. Should be between 0 and 1
 * @param explorationRate Probability that another than the best neighbour field will be chosen to move to
 * @param gamma Learning parameter according the one used paper
 */
private[antDefenseAIs] class LasiusNigerBehaviour implements (
 val antWorker: AntWorker,
 val emotionalDwellTime: Int = 5,
 val maxAggressiveness: Int = 20,
 val maxAggressivenessProb: Double = 0.767d,
 val minAggressivenessProb: Double = 0.257d,
 val alpha: Double = 0.98d,
 val explorationRate: Double = 0.3d,
 val gamma: Double = 0.98d) extends AntBehaviour {

  val world = antWorker.world
  val randomGenerator:MersenneTwisterFast = world.random

  ///////////////////// Common variables and constants /////////////////////////////////////

  private var boredom: Int = notBored /** 0 if an ant is „bored“ of searching abortively food and wants to go home */

  /** Last went direction */
  private var lastDirection: world.Direction.Value = {  // Initialise with random value
  val valueList = world.Direction.values.toList
    valueList.apply(world.random.nextInt(world.Direction.values.size))
  }

  /**
   * Possible emotional states of the ant
   */
  private object Emotion extends Enumeration {
    val aggressive = Value("Aggressive") /** Ant attacks other individuals in neighbourhood */
    val defensive = Value("Defensive") /** Ant flees into direction of its home  */

    /** Ant ignores ants of stranger colonies and changes emotional state if it receives a hit  */
    val normal = Value("Normal")

    /** Ant changes emotional state as soon as it sees ants of stranger colonies or receives a hit */
    val undecided = Value("Undecided")
  }

  private var emotion: Emotion.Value = Emotion.undecided /** Current emotional state */
  private var nextEmotionChange = emotionalDwellTime /** Time until the next state relaxation */


  ///////////////////// Behaviour description /////////////////////////////////////

  override def reactToHit() {
    if (emotion == Emotion.normal || emotion == Emotion.undecided) // If ant normal or undecided
      adaptState() // … calculate new state
  }

  override def step(state: SimState) {

    /*
     * If the ant is deeply neutral (i.e. emotion == 0) it adapts its state when there are more than
     * `threshold_strangers` ants of other colonies and more than `threshold_friends` ants of the own
     * colony in the neighbourhood. The first condition ensures that the ant does not change every simulation
     * step its behaviour.
     */
    val threshold_strangers = 1
    val threshold_friends = min(5, maxAggressiveness) // should be <= than `maxAggressiveness`

    if (emotion == Emotion.undecided && countStrangers() >= threshold_strangers && countFriends() >= threshold_friends)
      adaptState()

    emotion match {
      case Emotion.aggressive => actMilitarily()
      case Emotion.defensive => followHomeWay()
      case e if e == Emotion.normal || e == Emotion.undecided => actEconomically()
    }

    relax()
  }

  /**
   * The ant tries to pursuit and to hit ants of strange colonies.
   *
   * If an foreign ant is on own field, it will be hit. If there are no foreign ants on the own field but on an
   * neighbour field instead, one of them will be hit, preferably in the direction the ant went the last step.
   * If there are no enemies around, the ant will act economically.
   */
  override protected def actMilitarily() {
    def antNotOfSameColony(a: Ant): Boolean = a.tribeID != antWorker.tribeID

    val foreignAntsOnOwnField = antWorker.listAntsFullfillingPredicate(0)(antNotOfSameColony)
    if (foreignAntsOnOwnField.size > 0)
      antWorker.hit(foreignAntsOnOwnField.head)
    else {
      def directionContainsEnemy(dir: world.Direction.Value): Boolean = {
        val destiny = world.Direction.inDirection(antWorker.currentPos, dir)
        val foreignAntsInDirection = world.antsOn(destiny).filter(antNotOfSameColony)
        foreignAntsInDirection.size > 0
      }

      val directionsContainingEnemies = world.validDirections(antWorker).filter(directionContainsEnemy)
      if (directionsContainingEnemies.size > 0) {
        def directionSorter(dir: world.Direction.Value) = world.Direction.directionDistance(lastDirection, dir)

        antWorker.moveTo(directionsContainingEnemies.sortBy(directionSorter).head)
        val foreignAntsOnNewField = world.antsOn(antWorker.currentPos).filter(antNotOfSameColony)
        antWorker.hit(foreignAntsOnNewField.head)

      } else
        actEconomically()
    }
  }

  /** Actions for ants serving the economy of its tribe.
    *
    * If the backpack is full, or the ant is bored that is the ant has searched too long resources
    * without success, the ant follows the way home to its queen and give all resources in the backpack
    * to her. (After that ant is not bored at all.)
    *
    * In any other case the ant cares for food.
    */
  final private[model] def actEconomically() {

    val backpack_full: Boolean = antWorker.inBackpack >= AntWorker.backpack
    val isBored: Boolean = boredom == 0

    if (backpack_full || isBored) {
      if (currentPos == myQueen.currentPos) { // queen is under the ant
        dropResources()
        boredom = notBored
      }
      else
        followHomeWay()

    }
    else
      careForFood()
  }

  /**
   * Follow home way.
   *
   * The next field is most probable one of the neighbour-fields with the best home-pheromones.
   * With a certain probability (in function of the world.explorationRate) it is one of the other fields.
   */
  private def followHomeWay() {
    val direction = chooseDirectionByPheromone(homePheroOn)
    moveTo(direction)
    adaptHomePhero()
    adaptResPhero()
  }

  /**
   * Care for food.
   *
   * The next field is most probable one of the neighbour-fields with the best resource-pheromones.
   * With a certain probability (in function of the world.explorationRate) it is one of the other fields
   */
  private def careForFood() {
    val direction = chooseDirectionByPheromone(resPheroOn)
    moveTo(direction)
    adaptHomePhero()
    adaptResPhero()
    mineRes()
  }

  private def relax() {
    if (nextEmotionChange <= 0)
      emotion match {
        case Emotion.aggressive => emotion = Emotion.normal; nextEmotionChange = emotionalDwellTime
        case Emotion.defensive => emotion = Emotion.normal; nextEmotionChange = emotionalDwellTime
        case Emotion.normal => emotion = Emotion.undecided
        case Emotion.undecided => // do nothing
      }

    else
      nextEmotionChange -= 1
  }

  ///////////////////// Behaviour helpers /////////////////////////////////////

  /**
   * Adapts the emotional state of the ant.
   *
   * Changes either to aggressive state or to defensive state, in function of the number of ants
   * of the same colony in the nearby environment.
   *
   * It is assumed that the chance that an ant gets aggressive grows linearly.
   */
  def adaptState() {
    val alpha = min(1, countFriends() / maxAggressiveness)
    val aggressivenessProb = alpha * maxAggressivenessProb + (1 - alpha) * minAggressivenessProb
    emotion = if (randomGenerator.nextDouble() <= aggressivenessProb) Emotion.aggressive else Emotion.defensive
    nextEmotionChange = emotionalDwellTime
  }

  /**
   * Chooses a neighbourfield to go to.
   *
   * Works in the following way: With a probability of (1 - `explorationRate`) the position with
   * the best evaluation is chosen. In the other case there will be chosen a random position
   * from the rest of the fields.
   *
   * @param pheroOn Pheromone to observe for determining next position.
   * @return
   */
  final def chooseDirectionByPheromone(pheroOn: ((Int, Int)) => Double): world.Direction.Value = {
    def valueDirection = valueDirectionWithPhero(pheroOn) _

    // Add to every direction its value
    val directionsValued: List[(world.Direction.Value, Double)] =
      validDirections.map(x => (x, valueDirection(x)))

    val valDirsSorted = directionsValued.sortBy(x => x._2).reverse

    if (world.random.nextDouble() <= (1.0d - explorationRate))
      valDirsSorted.head._1
    else
      valDirsSorted.apply( 1 + world.random.nextInt(valDirsSorted.size - 1))._1
  }

  /**
   * Counts the number of ants of the same colony within the neighbourhood.
   *
   * The size of the observed neighbourhood is indicated by `antsSensingRange`.
   *
   * @return Number of ants of the same colony in the neighbourhood
   */
  def countFriends(): Int =
    antWorker.countAntsFullfillingPredicate(antsSensingRange)(a => a.tribeID == antWorker.tribeID)

  /**
   * Counts the number of ants of other colonies within the neighbourhood.
   *
   * The size of the observed neighbourhood is indicated by `antsSensingRange`.
   *
   * @return Number of ants of other colonies in the neighbourhood
   */
  def countStrangers(): Int =
    antWorker.countAntsFullfillingPredicate(antsSensingRange)((a: Ant) => a.tribeID != antWorker.tribeID)
}