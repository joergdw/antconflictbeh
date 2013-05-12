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
 * @param warPheroThreshold Ant recognises war pheromone values from this value on
 * @param alpha Influence of pheromone for determine next position. Should be between 0 and 1
 * @param explorationRate Probability that another than the best neighbour field will be chosen to move to
 * @param gamma Learning parameter according the one used paper
 */
class ArtificialAntBehaviourConf(
  val emotionalDwellTime: Int = 10,
  val warPheroThreshold: Double = 0.1e-3,
  override val alpha: Double = 0.98d,
  override val explorationRate: Double = 0.3d,
  override val gamma: Double = 0.98d) extends BehaviourConf(alpha, explorationRate, gamma)


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
  override val behaviourConf: ArtificialAntBehaviourConf) extends AntWorker(tribeID, world, behaviourConf) {
  import behaviourConf._

  /**
   * Constructs ant with the information of the given ant
   *
   * @param ant Ant giving the information of construction
   * @return Ant of the same colony in the same simulation
   */
  def this(ant: Ant, behaviourConf: ArtificialAntBehaviourConf) = this(ant.tribeID, ant.world, behaviourConf)

  ///////////////////// Common variables and constants /////////////////////////////////////

  val notBored: Int = 100 /** Value of boredom, 100 if an ant is not bored at all */
  private var boredom: Int = notBored /** 0 if an ant is „bored“ of searching abortively food and wants to go home */

  private object Emotion extends Enumeration {
    val battlesome = Value("Battlesome")
    val normal = Value("Normal")
    val fearsome = Value("Fearsome")
  }
  private var emotion: Emotion.Value = Emotion.normal /** Current emotional state */
  private var nextEmotionChange = emotionalDwellTime /** Time until the next state relaxation */


  //////////////////// Basic operations ////////////////////////////////


  /**
   * Evaluates the relationship in the area `antsSensingRange`
   *
   * @return < 1 iff ants from foreign colonies outnumber the ones from the own, (>= 1 else)
   */
  def evalueSituation(): Double = countFriends() / countStrangers()

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
   * Counts the number of ants within the neighbourhood fulfilling a predicate.
   *
   * The size of the observed neighbourhood is indicated by `antsSensingRange`.
   *
   * @param range Range in which will be searched
   * @param p Predicate
   * @return Number of ants in the neighbourhood fulfilling the predicate p
   */
  def countAntsFullfillingPredicate(range: Int)(p: Ant => Boolean): Int = {
    val ants: List[Ant] = neighbourhood(range).map(world.antsOn).flatten
    def adder(i: Int, a: Ant): Int = i + (if (p(a)) 1 else 0)
    ants.foldLeft(0: Int)(adder)
  }

  /**
   * Adapts the war pheromones of the current field.
   */
  def adaptWarPhero() {
    val bestNeighbour: (Int, Int) = nearPos(1).sortBy(warPheroOn).reverse.head
    val adaptedValue = gamma * homePheroOn(bestNeighbour)

    setWarPheroOn(currentPos, min(1, adaptedValue))
  }


  ///////////////////// Behaviour description /////////////////////////////////////

  def step(state: SimState) {

    emotion match {
      case Emotion.normal => {
        val warPheroDir = chooseDirectionByPheromone(warPheroOn)
        val bestWarPhero = warPheroOn(world.Direction.inDirection(currentPos, warPheroDir))

        if (bestWarPhero >= warPheroThreshold) {
          if (bestWarPhero < warPheroOn(currentPos))
            emotion = Emotion.battlesome
          else
            moveTo(warPheroDir)
        }
        else
          actEconomically()
      }
      case Emotion.fearsome => {
        if (currentPos == myQueen.currentPos)
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

      val directionsContainingEnemies = world.validDirections(this).filter(directionContainsEnemy)
      if (directionsContainingEnemies.size > 0) {
        def directionSorter(dir: world.Direction.Value) = world.Direction.directionDistance(lastDirection, dir)

        moveTo(directionsContainingEnemies.sortBy(directionSorter).head)
        val foreignAntsOnNewField = world.antsOn(currentPos).filter(a => a.tribeID != tribeID)
        hit(foreignAntsOnNewField.head)

      } else {
       val dir = chooseDirectionByPheromone(p => 0.0d) // Pheromones don't matter, (but old directions still do)
       moveTo(dir)
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
  final protected def followHomeWay() {
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
  final protected def careForFood() {
    val direction = chooseDirectionByPheromone(resPheroOn)
    moveTo(direction)
    adaptHomePhero()
    adaptResPhero()
    mineRes()
  }

  def adaptEmotion() {
    if (emotion == Emotion.battlesome && nextEmotionChange <= 0) {
      emotion = Emotion.normal
      nextEmotionChange = emotionalDwellTime
    }
    else if (emotion == Emotion.battlesome)
      nextEmotionChange -= 1
  }

  override def receiveHit(opponent: Ant) {
    super.receiveHit(opponent)
    if (isKilled) return // Ant dead: no more actions

    // Adapt emotion
    emotion = if (evalueSituation() < 1) Emotion.fearsome else Emotion.battlesome
  }
}