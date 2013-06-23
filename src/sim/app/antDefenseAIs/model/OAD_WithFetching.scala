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

private[antDefenseAIs] class OAD_WithFetching_Generator(
  override val behaviourConf: OAD_BehaviourConf)
  extends AntGenerator {

  /**
   * Creates an NormalAntWorker
   *
   * @param tribeID Tribe the ant belongs to
   * @param world World the ant lives on
   * @return NormalAntWorker
   */
  def apply(tribeID: Int, world: World, beh: OAD_BehaviourConf) =
    new OpportunisticAggressiveDefender(tribeID, world, beh)

  def apply(ant: Ant) =
    behaviourConf match {
      case c: OAD_BehaviourConf => new OpportunisticAggressiveDefender(ant, behaviourConf)
      case _                    => throw new IllegalArgumentException("Configuration not of required type.")
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
class OAD_WithFetching_BehaviourConf(
  val emotionalDwellTime: Int = 10,
  override val alpha: Double = 0.98d,
  override val explorationRate: Double = 0.3d,
  override val gamma: Double = 0.98d,
  val notBored: Int = 500)
  extends BehaviourConf(alpha, explorationRate, gamma)


import sim.engine.SimState

/**
 * AntWorker with a more offensive behaviour
 *
 * @param tribeID Tribe the ant belongs to
 * @param world World the ant lives on
 */
private[antDefenseAIs] class OAD_WithFetching(
  override val tribeID: Int,
  override val world: World,
  val behaviourConf: OAD_BehaviourConf)
  extends AntWorker with StandardPheroSystem with EconomicStandardBehaviour {

  import behaviourConf._

  override val alpha = behaviourConf.alpha
  override val explorationRate = behaviourConf.explorationRate
  override val gamma = behaviourConf.gamma
  override val notBored = behaviourConf.notBored


  /**
   * Constructs ant with the information of the given ant
   *
   * @param ant Ant giving the information of construction
   * @return Ant of the same colony in the same simulation
   */
  def this(ant: Ant, behaviourConf: OAD_BehaviourConf) = this(ant.tribeID, ant.world, behaviourConf)

  ///////////////////// Common variables and constants /////////////////////////////////////

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


  ///////////////////// Behaviour description /////////////////////////////////////

  def step(state: SimState) {

    emotion match {
      case Emotion.normal => {
        val warPheroDir = chooseDirectionBy(valueDirectionWithFunction(warPheroOf))

        if (warPheroDir.isDefined) {
          val bestWarPhero = warPheroOf(warPheroDir.get)

          if (bestWarPhero > 0) {
            if (bestWarPhero < warPheroOf()) { // End of phero route reached
              emotion = Emotion.battlesome
              actMilitarily()
            }
            else {
              moveTo(warPheroDir.get)
              adaptAllPheros()
            }
          }
          else
            actEconomically()
        }
      }
      case Emotion.fearsome => followHomeWay()
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
      def directionContainsEnemy(dir: Direction.Value): Boolean = {
        val destiny = Direction.inDirection(currentPos, dir)
        val foreignAntsInDirection = world.antsOn(destiny).filter(a => a.tribeID != tribeID)
        foreignAntsInDirection.size > 0
      }

      val validDirs = validDirections

      val directionsContainingEnemies = validDirs.filter(directionContainsEnemy)
      if (directionsContainingEnemies.size > 0) {
        def directionSorter(dir: Direction.Value) = Direction.directionDistance(lastDirection, dir)

        moveTo(directionsContainingEnemies.sortBy(directionSorter).head)
        adaptAllPheros()
        val foreignAntsOnNewField = world.antsOn(currentPos).filter(a => a.tribeID != tribeID)
        hit(foreignAntsOnNewField.head)

      } else {
        val dir = validDirs(world.random.nextInt(validDirs.size)) // Choose totally random direction
        moveTo(dir)
        adaptAllPheros()
      }
    }
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
      case Emotion.fearsome => {
        val queenPos = world.currentPosOf(myQueen)
        if (queenPos.isDefined && currentPos == queenPos.get)
          emotion = Emotion.normal
      }
    }
  }

  override def receiveHitFrom(opponent: Ant) {
    super.receiveHitFrom(opponent)

    // Adapt emotion
    emotion = if (evalueSituation().get < 1) Emotion.fearsome else Emotion.battlesome
    if (emotion == Emotion.fearsome) // Start war pheromone route
      setWarPhero(1)
  }
}
