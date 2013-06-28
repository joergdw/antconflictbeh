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
 * @param notBored Value of boredom if the ant is not bored at all
 */
private[antDefenseAIs] class LN_BehaviourConf(
  val emotionalDwellTime: Int = 10,
  val maxAggressiveness: Int = 20,
  val maxAggressivenessProb: Double = 0.767d,
  val minAggressivenessProb: Double = 0.257d,
  override val alpha: Double = 0.98d,
  override val explorationRate: Double = 0.3d,
  override val gamma: Double = 0.98d,
  val notBored: Int = 500) extends BehaviourConf(alpha, explorationRate, gamma)


private[antDefenseAIs] class LN_Generator(
  override val behaviourConf: LN_BehaviourConf) extends AntGenerator {

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
      case c: LN_BehaviourConf => new LasiusNiger(ant, behaviourConf)
      case _                      => throw new IllegalArgumentException("Configuration not of required type.")
    }
}


import StrictMath.min

import sim.engine.SimState

/**
 * Antworker with the usual strategies
 *
 * @param tribeID Tribe the ant belongs to
 * @param world World the ant lives on
 */
private[antDefenseAIs] class LasiusNiger(
  override val tribeID: Int,
  override val world: World,
  val behaviourConf: LN_BehaviourConf)
  extends AntWorker with StandardPheroSystem with EconomicStandardBehaviour with CooldownConflictBehaviour {

  // Initialise configuration
  import behaviourConf._
  override val alpha = behaviourConf.alpha
  override val explorationRate = behaviourConf.explorationRate
  override val gamma = behaviourConf.gamma
  override val notBored = behaviourConf.notBored
  override val emotionalDwellTime = behaviourConf.emotionalDwellTime

  /**
   * Constructs ant with the information of the given ant
   *
   * @param ant Ant ant giving the information of construction
   * @return Ant of the same colony in the same simulation
   */
  def this(ant: Ant, behaviourConf: LN_BehaviourConf) = this(ant.tribeID, ant.world, behaviourConf)


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


  ///////////////////// Behaviour description /////////////////////////////////////

  /**
   * Ant acts based on its current emotional state.
   *
   * @param state Parameter not used
   */
  override def step(state: SimState) {

    /*
     * If the ant is deeply neutral (i.e. emotion == 0) it adapts its state when there are more than
     * `threshold_strangers` ants of other colonies in the neighbourhood. This ensures that the ant does
     * not change every simulation step its behaviour.
     */
    val threshold_strangers = 1

    if (emotion == Emotion.undecided && countStrangers() >= threshold_strangers)
      adaptState()

    emotion match {
      case Emotion.aggressive => if (enemyClose()) attackNearEnemy() else actEconomically()
      case Emotion.defensive => followHomeWay()
      case e if e == Emotion.normal || e == Emotion.undecided => actEconomically()
    }

    relax()
  }

  override def receiveHitFrom(opponent: Ant) {
    super.receiveHitFrom(opponent)

    if (emotion == Emotion.normal || emotion == Emotion.undecided) // If ant normal or undecided
      adaptState() // … calculate new state
  }
}