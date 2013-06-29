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

import sim.engine.SimState

/**
 * Behaviour configuration of an Lasius Niger ant ant
 *
 * @param emotionalDwellTime How long an individual stays in an individual state before going to another
 * @param notBored Value of boredom if the ant is not bored at all
 */
private[antDefenseAIs] class LN_BehaviourConf(
  val emotionalDwellTime: Int,
  val notBored: Int,
  override val alpha: Double,
  override val explorationRate: Double,
  override val gamma: Double)
  extends BehaviourConf(alpha, explorationRate, gamma)

/**
 * AntWorker with the usual strategies.
 *
 * Differs only in emotion changing behaviour.
 *
 * @param tribeID Tribe the ant belongs to
 * @param world World the ant lives on
 */
private[antDefenseAIs] abstract class LasiusNiger(
  override val tribeID: Int,
  override val world: World,
  val behaviourConf: LN_BehaviourConf)
  extends AntWorker with StandardPheroSystem with EconomicStandardBehaviour with ConflictBehaviour {

  //------------------------------ Initialise configuration ----------------------------------------
  override val alpha = behaviourConf.alpha
  override val explorationRate = behaviourConf.explorationRate
  override val gamma = behaviourConf.gamma
  override val notBored = behaviourConf.notBored

  /** How long an individual stays in an individual state before going to another */
  val emotionalDwellTime: Int = behaviourConf.emotionalDwellTime


  /**
   * Constructs ant with the information of the given ant
   *
   * @param ant Ant ant giving the information of construction
   * @return Ant of the same colony in the same simulation
   */
  def this(ant: Ant, behaviourConf: LN_Normal_BehaviourConf) = this(ant.tribeID, ant.world, behaviourConf)

  /**
   * Possible emotional states of an ant
   */
  protected[this] object Emotion extends Enumeration {
    val aggressive = Value("Aggressive") /** Ant attacks near foreign-colony ants */
    val fleeing = Value("Fleeing") /** Ant flees into direction of its home  */

    /** Ant ignores ants of stranger colonies and changes emotional state if it receives a hit  */
    val normal = Value("Normal")

    /** Ant changes emotional state as soon as it sees ants of stranger colonies or receives a hit */
    val undecided = Value("Undecided")
  }

  protected[this] var emotion: Emotion.Value = Emotion.undecided /** Current emotional state */
  protected[this] var nextEmotionChange = emotionalDwellTime /** Time until the next state relaxation */

  /**
   * Cools down the emotional state of the ant until `undecided`
   */
  protected[this] def relax() {
    if (nextEmotionChange <= 0)
      emotion = Emotion.undecided
    else
      nextEmotionChange -= 1
  }


  //-------------------------- (Additional) Basic operations ----------------------------------------

  /**
   * Adapts the emotional state of the ant by changing it either to aggressive state, normal state
   * or to defensive state.
   */
  def adaptState()


  //------------------------- Behaviour description -----------------------------------------------

  /**
   * Ant acts based on its current emotional state.
   *
   * @param state Parameter not used
   */
  override def step(state: SimState) {
    if (emotion == Emotion.undecided && enemyClose())
      adaptState()

    emotion match {
      case Emotion.aggressive => if (enemyClose()) attackNearEnemy() else actEconomically()
      case Emotion.fleeing => followHomeWay()
      case e if e == Emotion.normal || e == Emotion.undecided => actEconomically()
    }

    relax()
  }
}
