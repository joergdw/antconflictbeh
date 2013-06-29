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

private[antDefenseAIs] class OAD_Generator(
  override val behaviourConf: OAD_BehaviourConf) extends AntGenerator {

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
class OAD_BehaviourConf(
  val emotionalDwellTime: Int = 10,
  override val alpha: Double = 0.98d,
  override val explorationRate: Double = 0.3d,
  override val gamma: Double = 0.98d,
  val notBored: Int = 500) extends BehaviourConf(alpha, explorationRate, gamma)


import sim.engine.SimState

/**
 * AntWorker with a more offensive behaviour
 *
 * @param tribeID Tribe the ant belongs to
 * @param world World the ant lives on
 */
private[antDefenseAIs] class OpportunisticAggressiveDefender(
  override val tribeID: Int,
  override val world: World,
  val behaviourConf: OAD_BehaviourConf)
  extends AntWorker with StandardPheroSystem with EconomicStandardBehaviour with CooldownConflictBehaviour {

  override val alpha = behaviourConf.alpha
  override val explorationRate = behaviourConf.explorationRate
  override val gamma = behaviourConf.gamma
  override val notBored = behaviourConf.notBored
  override val emotionalDwellTime = behaviourConf.emotionalDwellTime

  /**
   * Constructs ant with the information of the given ant
   *
   * @param ant Ant giving the information of construction
   * @return Ant of the same colony in the same simulation
   */
  def this(ant: Ant, behaviourConf: OAD_BehaviourConf) = this(ant.tribeID, ant.world, behaviourConf)

  //------------------- Common variables and constants -------------------------------------


  //------------------- (Additional) Basic operations --------------------------------------

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


  //-------------------------------- Behaviour description -------------------------------------------

  def step(state: SimState) {
    emotion match {
      case Emotion.undecided => actEconomically()
      case Emotion.defensive => followHomeWay()
      case Emotion.aggressive => if (enemyClose()) attackNearEnemy() else actEconomically()
    }

    adaptState()
  }

  def adaptState() {
    emotion = evalueSituation() match {
      case None    => Emotion.undecided
      case Some(d) => if (d > 1) Emotion.aggressive else Emotion.undecided
    }
    nextEmotionChange = emotionalDwellTime
  }

  override def receiveHitFrom(opponent: Ant) {
    super.receiveHitFrom(opponent)

    // Adapt emotion
    emotion = if (evalueSituation().get < 1) Emotion.defensive else Emotion.aggressive
  }
}