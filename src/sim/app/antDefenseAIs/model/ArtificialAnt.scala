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

private[antDefenseAIs] class ArtificialAntGenerator(
  override val behaviourConf: ArtificialAntBehaviourConf) extends AntGenerator {
  /**
   * Creates an NormalAntWorker
   *
   * @param tribeID Tribe the ant belongs to
   * @param world World the ant lives on
   * @return NormalAntWorker
   */
  def apply(tribeID: Int, world: World, beh: ArtificialAntBehaviourConf) = new ArtificialAnt(tribeID, world, beh)

  def apply(ant: Ant): AntWorker = {
    if (behaviourConf.isInstanceOf[ArtificialAntBehaviourConf])
      new ArtificialAnt(ant, behaviourConf.asInstanceOf[ArtificialAntBehaviourConf])

    else
      throw new IllegalArgumentException("Configuration not of required type.")
  }
}

/**
 * Behaviour configuration of an Artificial-Ant ant
 *
 * @param emotionalDwellTime How long an individual stays in the battlesome emotional state before
 *                           changing to a normal state
 * @param alpha Influence of pheromone for determine next position. Should be between 0 and 1
 * @param explorationRate Probability that another than the best neighbour field will be chosen to move to
 * @param gamma Learning parameter according the one used paper
 * @param notBored Value of boredom if the ant is not bored at all
 */
class ArtificialAntBehaviourConf(
  val emotionalDwellTime: Int = 10,
  override val alpha: Double = 0.98d,
  override val explorationRate: Double = 0.3d,
  override val gamma: Double = 0.98d,
  val notBored: Int = 500) extends BehaviourConf(alpha, explorationRate, gamma)


private[antDefenseAIs] object ArtificialAnt {
  val antsSensingRange: Int = 3 /** Radius of the area the ant can sense other individuals */
}


import sim.engine.SimState

import ArtificialAnt._
import java.lang.StrictMath._

/**
 * AntWorker with a more offensive behaviour
 *
 * @param tribeID Tribe the ant belongs to
 * @param world World the ant lives on
 */
private[antDefenseAIs] class ArtificialAnt(
  override val tribeID: Int,
  override val world: World,
  val behaviourConf: ArtificialAntBehaviourConf) extends AntWorker(tribeID, world) {
  import behaviourConf._

  /**
   * Constructs ant with the information of the given ant
   *
   * @param ant Ant giving the information of construction
   * @return Ant of the same colony in the same simulation
   */
  def this(ant: Ant, behaviourConf: ArtificialAntBehaviourConf) = this(ant.tribeID, ant.world, behaviourConf)

  ///////////////////// Common variables and constants /////////////////////////////////////

  private var boredom: Int = notBored /** 0 if an ant is „bored“ of searching abortively food and wants to go home */

  private object Emotion extends Enumeration {
    val battlesome = Value("Battlesome")
    val normal = Value("Normal")
    val fearsome = Value("Fearsome")
  }
  private var emotion: Emotion.Value = Emotion.normal /** Current emotional state */
  private var nextEmotionChange = emotionalDwellTime /** Time until the next state relaxation */


  //////////////////// (Additional) Basic operations ////////////////////////////////


  /**
   * Evaluates the relationship in the area `antsSensingRange`
   *
   * @return `Some` < 1 iff ants from foreign colonies outnumber the ones from the own, (>= 1 else) – if no strangers in
   *         the neighbourhood `None will be returned`.
   */
  def evalueSituation(): Option[Double] = {
    val strangers = countStrangers()

    if (strangers == 0)
      None
    else
      Some(countFriends() / strangers)
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

  /**
   * Adapts the war pheromones of the current field.
   */
  def adaptWarPhero() {
    val bestNeighbour: world.Direction.Value = validDirections.sortBy(warPheroOf).reverse.head
    val adaptedValue = gamma * warPheroOf(bestNeighbour)

    setWarPhero(min(warPheroOf(), adaptedValue))
  }


  ///////////////////// Behaviour description /////////////////////////////////////

  def step(state: SimState) {

    emotion match {
      case Emotion.normal => {
        val warPheroDir = chooseDirectionBy(warPheroOf)
        val bestWarPhero = warPheroOf(warPheroDir)

        if (bestWarPhero > 0) {
          if (bestWarPhero < warPheroOf()) { // End of phero route reached
            emotion = Emotion.battlesome
            actMilitarily()
          }
          else
            moveTo(warPheroDir)
            adaptHomePhero()
            adaptResPhero()
        }
        else
          actEconomically()
      }
      case Emotion.fearsome => {
        val queenPos = world.currentPos(myQueen)
        if (queenPos.isDefined && currentPos == queenPos.get)
          emotion = Emotion.normal
        else {
          followHomeWay()
          adaptWarPhero()
        }
      }
      case Emotion.battlesome => actMilitarily()
    }

    adaptEmotion()
  }

  /**
   * The ant tries to pursuit and to hit ants of strange colonies.
   *
   * If an foreign ant is on own field, it will be hit. If there are no foreign ants on the own field but on an
   * neighbour field instead, one of them will be hit, preferably in the direction the ant went the last step.
   * If there are no enemies around, the ant will move into a direction.
   */
  def actMilitarily() {

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
        val foreignAntsOnNewField = world.antsOn(currentPos).filter(a => a.tribeID != tribeID)
        hit(foreignAntsOnNewField.head)

      } else {
       val dir = validDirs(world.random.nextInt(validDirs.size)) // Choose totally random direction
       moveTo(dir)
       adaptHomePhero()
       adaptResPhero()
      }
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
  final protected def actEconomically() {
    val backpack_full: Boolean = transporting >= AntWorker.backpackSize
    val isBored: Boolean = boredom == 0

    if (backpack_full || isBored) {
      val queenPos: Option[(Int, Int)] = world.currentPos(myQueen)
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
   * Follow home way.
   *
   * The next field is most probable one of the neighbour-fields with the best home-pheromones.
   * With a certain probability (in function of the world.explorationRate) it is one of the other fields.
   */
  final protected def followHomeWay() {
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
  final protected def careForFood() {
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

    val adaptedValue = world.currentPos(myQueen) match {
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
    val adaptedValue = (world.resOn(currentPos) + gamma * resPheroOf(bestNeighbour) / world.maxResAmount)

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

    if (world.random.nextDouble() <= (1.0d - explorationRate))
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

  def adaptEmotion() {
    emotion match {
      case Emotion.battlesome if nextEmotionChange <= 0 => {
        emotion = Emotion.normal
        nextEmotionChange = emotionalDwellTime
      }
      case Emotion.battlesome => nextEmotionChange -= 1
      case Emotion.normal => evalueSituation() match {
        case None => // Do noting because no strangers in the area
        case Some(n) => if (n >= 1) emotion = Emotion.battlesome
      }
      case Emotion.fearsome => // Do nothing
    }
  }

  override def receiveHit(opponent: Ant) {
    super.receiveHit(opponent)

    // Adapt emotion
    emotion = if (evalueSituation().get < 1) Emotion.fearsome else Emotion.battlesome
    if (emotion == Emotion.fearsome) // Start war pheromone route
      setWarPhero(1)
  }

  /**
   * Mines, if possible, resources. Boredom increased if no resources.
   * No boredom if try successful.
   */
  override def mineRes() {
    val tmp = transporting
    super.mineRes()

    if (transporting > tmp) // successfull mined
      boredom = notBored
    else
      boredom -= 1
  }
}