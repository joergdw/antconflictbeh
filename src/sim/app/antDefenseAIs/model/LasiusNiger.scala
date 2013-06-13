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


/**
 * Behaviour configuration of an Lasius Niger ant ant
 *
 * @param emotionalDwellTime How long an individual stays in an individual state before going to another
 * @param maxAggressiveness As of this value of other ants of the same colony, the ant changes state with probability
 *                          `maxAggressivenessProb`.
 * @param maxAggressivenessProb Highest possible probability that an ant gets aggressive
 * @param minAggressivenessProb Lowest possible probability that an ant gets aggressive
 * @param alpha Influence of pheromone for determine next position. Should be between 0 and 1
 * @param explorationRate Probability that another than the best neighbour field will be chosen to move to
 * @param gamma Learning parameter according the one used paper
 * @param notBored Value of boredom if the ant is not bored at all
 */
private[antDefenseAIs] class LasiusBehaviourConf(
  val emotionalDwellTime: Int = 10,
  val maxAggressiveness: Int = 20,
  val maxAggressivenessProb: Double = 0.767d,
  val minAggressivenessProb: Double = 0.257d,
  override val alpha: Double = 0.98d,
  override val explorationRate: Double = 0.3d,
  override val gamma: Double = 0.98d,
  val notBored: Int = 500) extends BehaviourConf(alpha, explorationRate, gamma)


private[antDefenseAIs] class LasiusNigerGenerator(
  override val behaviourConf: LasiusBehaviourConf) extends AntGenerator {

  /**
   * Creates an NormalAntWorker
   *
   * @param tribeID Tribe the ant belongs to
   * @param world World the ant lives on
   * @return NormalAntWorker
   */
  def apply(tribeID: Int, world: World) = new LasiusNiger(tribeID, world, behaviourConf)

  def apply(ant: Ant) =
    behaviourConf match {
      case c: LasiusBehaviourConf => new LasiusNiger(ant, behaviourConf)
      case _                      => throw new IllegalArgumentException("Configuration not of required type.")
    }
}

private[antDefenseAIs] object LasiusNiger {
  val antsSensingRange: Int = 2 /** Radius of the area the ant can sense other individuals */
}


import StrictMath.min

import sim.engine.SimState

import LasiusNiger._
import java.lang.StrictMath._

/**
 * Antworker with the usual strategies
 *
 * @param tribeID Tribe the ant belongs to
 * @param world World the ant lives on
 */
private[antDefenseAIs] class LasiusNiger(
  override val tribeID: Int,
  override val world: World,
  val behaviourConf: LasiusBehaviourConf) extends AntWorker(tribeID, world) {
  import behaviourConf._

  /**
   * Constructs ant with the information of the given ant
   *
   * @param ant Ant ant giving the information of construction
   * @return Ant of the same colony in the same simulation
   */
  def this(ant: Ant, behaviourConf: LasiusBehaviourConf) = this(ant.tribeID, ant.world, behaviourConf)


  ///////////////////// Common variables and constants /////////////////////////////////////

  private var boredom: Int = notBored /** 0 if an ant is „bored“ of searching abortively food and wants to go home */

  /**
   * Possible emotional states of an ant
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


  ///////////////////// (Additional) Basic operations /////////////////////////////////////

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
    emotion = if (world.random.nextDouble() <= aggressivenessProb) Emotion.aggressive else Emotion.defensive
    nextEmotionChange = emotionalDwellTime
  }

  /**
   * Counts the number of ants of the same colony within the neighbourhood.
   *
   * The size of the observed neighbourhood is indicated by `antsSensingRange`.
   *
   * @return Number of ants of the same colony in the neighbourhood
   */
  def countFriends(): Int = countAntsFullfillingPredicate(antsSensingRange)(a => a.tribeID == this.tribeID)

  /**
   * Counts the number of ants of other colonies within the neighbourhood.
   *
   * The size of the observed neighbourhood is indicated by `antsSensingRange`.
   *
   * @return Number of ants of other colonies in the neighbourhood
   */
  def countStrangers(): Int = countAntsFullfillingPredicate(antsSensingRange)((a: Ant) => a.tribeID != this.tribeID)


  ///////////////////// Behaviour description /////////////////////////////////////

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

  /** Actions for ants serving the economy of its tribe.
    *
    * If the backpack is full, or the ant is bored that is the ant has searched too long resources
    * without success, the ant follows the way home to its queen and give all resources in the backpack
    * to her. (After that ant is not bored at all.)
    *
    * In any other case the ant cares for food.
    */
  protected def actEconomically() {
    val backpack_full: Boolean = _inBackpack >= AntWorker.backpackSize
    val isBored: Boolean = boredom == 0

    if (backpack_full || isBored) {
      val queenPos: Option[(Int, Int)] = world.currentPosOf(myQueen)
      if (queenPos.isDefined && currentPos == queenPos.get) { // queen is under the ant
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
   * The ant tries to pursuit and to hit ants of strange colonies.
   *
   * If an foreign ant is on own field, it will be hit. If there are no foreign ants on the own field but on an
   * neighbour field instead, one of them will be hit, preferably in the direction the ant went the last step.
   * If there are no enemies around, the ant will act economically.
   */
  protected def actMilitarily() {

    val foreignAntsOnOwnField = world.antsOn(currentPos).filter(a => a.tribeID != tribeID)
    if (foreignAntsOnOwnField.size > 0)
      hit(foreignAntsOnOwnField.head)
    else {
      def directionContainsEnemy(dir: world.Direction.Value): Boolean = {
        val destiny = world.Direction.inDirection(currentPos, dir)
        val foreignAntsInDirection = world.antsOn(destiny).filter(a => a.tribeID != tribeID)
        foreignAntsInDirection.size > 0
      }

      val validDirs = validDirections
      val directionsContainingEnemies = validDirs.filter(directionContainsEnemy)
      if (directionsContainingEnemies.size > 0) {
        def directionSorter(dir: world.Direction.Value) = world.Direction.directionDistance(lastDirection, dir)

        moveTo(directionsContainingEnemies.sortBy(directionSorter).head)
        adaptHomePhero()
        adaptResPhero()
        val foreignAntsOnNewField = world.antsOn(currentPos).filter(a => a.tribeID != tribeID)
        hit(foreignAntsOnNewField.head)

      } else
        actEconomically()
    }
  }

  protected def relax() {
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


  override def receiveHitFrom(opponent: Ant) {
    super.receiveHitFrom(opponent)
    if (this.isKilled) return // Ant dead: no more actions

    if (emotion == Emotion.normal || emotion == Emotion.undecided) // If ant normal or undecided
      adaptState() // … calculate new state
  }

  /**
   * Follow home way.
   *
   * The next field is most probable one of the neighbour-fields with the best home-pheromones.
   * With a certain probability (in function of the world.explorationRate) it is one of the other fields.
   */
  protected def followHomeWay() {
    val direction = chooseDirectionBy(valueDirectionWithPhero(homePheroOf))
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
  protected def careForFood() {
    val direction = chooseDirectionBy(valueDirectionWithPhero(resPheroOf))
    moveTo(direction)
    adaptHomePhero()
    adaptResPhero()
    mineRes()
  }

  /**
   * Adapts the home-pheromones of the current field.
   */
  def adaptHomePhero() {
    val bestNeighbour: world.Direction.Value = validDirections.sortBy(homePheroOf).reverse.head

    val adaptedValue = world.currentPosOf(myQueen) match {
      case None => 0 // queen is killed an there is no home
      case Some(qPos) if currentPos == qPos => 1.0d
      case _ => gamma * homePheroOf(bestNeighbour)
    }

    // To avoid pheromone value > 1 and worse value than before
    setHomePhero(min(1, max(homePheroOf(), adaptedValue)))
  }

  /**
   * Adapts the ressource-pheromones of the current field.
   */
  def adaptResPhero() {
    val bestNeighbour: world.Direction.Value = validDirections.sortBy(resPheroOf).reverse.head
    val adaptedValue = world.resOn(currentPos) + gamma * resPheroOf(bestNeighbour) / world.maxResAmount

    setResPhero(min(1, adaptedValue))
  }

  /**
   * Chooses a direction to go to.
   *
   * Works in the following way: With a probability of (1 - `explorationRate`) the (valid) direction with
   * the best evaluation is chosen. In the other case there will be chosen a random direction.
   *
   * @param evaluate Function to evaluate
   * @return Direction chosen
   */
  protected def chooseDirectionBy(evaluate: world.Direction.Value => Double): world.Direction.Value = {
    val directionsValued: List[(world.Direction.Value, Double)] =
      validDirections.map(dir => (dir, evaluate(dir))) // Add to every direction its value

    val valDirsSorted = directionsValued.sortBy(x => x._2).reverse // descending order

    if (world.random.nextDouble() <= 1.0d - explorationRate)
      valDirsSorted.head._1
    else
      valDirsSorted.apply(1 + world.random.nextInt(valDirsSorted.size - 1))._1
  }

  // Calculates an all over all value for a direction
  protected def valueDirectionWithPhero(pheroInDir: world.Direction.Value => Double)(dir: world.Direction.Value): Double = {

    // Calculates a normalized value of a direction influenced by the pheromone
    def dirValueByPhero(dir: world.Direction.Value): Double = {
      val bestPheroInNeighbourhood = validDirections.map(pheroInDir).max

      if (bestPheroInNeighbourhood == 0)
        0
      else
        pheroInDir(dir) / bestPheroInNeighbourhood
    }

    // Calculates a normalized value of a direction influenced by the last direction
    def dirValueByDir(dir: world.Direction.Value): Double =
      world.Direction.directionDistance(lastDirection, dir) / world.Direction.MaxDirDistance

    alpha * dirValueByPhero(dir) + (1 - alpha) * dirValueByDir(dir)
  }

  /**
   * Mines, if possible, resources. Boredom increased if no resources.
   * No boredom if try successful.
   */
  override def mineRes() {
    val tmp = _inBackpack
    super.mineRes()

    if (_inBackpack > tmp) // successfull mined
      boredom = notBored
    else
      boredom -= 1
  }
}