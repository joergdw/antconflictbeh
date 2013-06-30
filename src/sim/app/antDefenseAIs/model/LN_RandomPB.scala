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

import java.lang.StrictMath.min

/**
 * Behaviour configuration of an normal Lasius Niger ant
 *
 * @param maxAggressiveness As of this value for the relationship between other ants of the same colony and enemy-ants
 *                          , the ant changes state with probability `maxAggressivenessProb`.
 * @param minNeutralnessProb Lowest possible probability that an ant ignores others
 * @param maxNeutralnessProb Highest possible probability that an ant ignores others
 * @param minAggressivenessProb Lowest possible probability that an ant gets aggressive
 * @param maxAggressivenessProb Highest possible probability that an ant gets aggressive
 */
private[antDefenseAIs] class LN_RPB_BehaviourConf(
  val maxAggressiveness: Double = 6d,
  val minNeutralnessProb: Double = 0.33d,
  val maxNeutralnessProb: Double = 0.2d,
  val minAggressivenessProb: Double = 0.257d,
  val maxAggressivenessProb: Double = 0.767d,
  override val emotionalDwellTime: Int = 8,
  override val notBored: Int = 500,
  override val alpha: Double = 0.98d,
  override val explorationRate: Double = 0.3d,
  override val gamma: Double = 0.98d)
  extends LN_BehaviourConf(emotionalDwellTime, notBored, alpha, explorationRate, gamma)


private[antDefenseAIs] class LN_RPB_Generator(
  override val behaviourConf: LN_RPB_BehaviourConf) extends AntGenerator {

  /**
   * Creates an NormalAntWorker
   *
   * @param tribeID Tribe the ant belongs to
   * @param world World the ant lives on
   * @return NormalAntWorker
   */
  def apply(tribeID: Int, world: World) = new LN_RandomPB(tribeID, world, behaviourConf)

  def apply(ant: Ant) =
    behaviourConf match {
      case c: LN_RPB_BehaviourConf => new LN_RandomPB(ant, behaviourConf)
    }
}

private[antDefenseAIs] class LN_RandomPB(
  override val tribeID: Int,
  override val world: World,
  override val behaviourConf: LN_RPB_BehaviourConf)
  extends LasiusNiger(tribeID, world, behaviourConf) {
  // Initialise configuration
  import behaviourConf._


  /**
   * Constructs ant with the information of the given ant
   *
   * @param ant Ant ant giving the information of construction
   * @return Ant of the same colony in the same simulation
   */
  def this(ant: Ant, behaviourConf: LN_RPB_BehaviourConf) = this(ant.tribeID, ant.world, behaviourConf)


  /**
   * Adapts the emotional state of the ant.
   *
   * Changes either to aggressive state or to defensive state, in function of the number of ants
   * of the same colony in the nearby environment.
   *
   * It is assumed that the chance that an ant gets aggressive grows linearly.
   */
  override def adaptState() {
    val alpha:Double = min(1d, evalueSituation().get / maxAggressiveness)
    val aggressivenessProb = alpha * maxAggressivenessProb + (1 - alpha) * minAggressivenessProb
    val neutralnessProb = alpha * maxNeutralnessProb + (1 - alpha) * minNeutralnessProb
    assert (aggressivenessProb + neutralnessProb <= 1)

    val random = world.random.nextDouble()
    emotion = if (random <= aggressivenessProb)
      Emotion.aggressive
    else if (random <= aggressivenessProb + neutralnessProb)
      Emotion.normal
    else
      Emotion.fleeing

    nextEmotionChange = emotionalDwellTime
  }


  //--------------------------------- Behaviour description --------------------------------------------

  override def receiveHitFrom(opponent: Ant) {
    super.receiveHitFrom(opponent)

    // If ant normal or undecided there are now only two choices to adapt the state
    if (emotion != Emotion.aggressive || emotion != Emotion.fleeing) {
      val alpha: Double = min(1d, evalueSituation().get / maxAggressiveness)
      val minAggProb: Double = (minNeutralnessProb + minAggressivenessProb) / 2
      val maxAggProb: Double = (maxNeutralnessProb + maxAggressivenessProb) / 2
      val aggProb: Double = alpha * maxAggProb + (1 - alpha) * minAggProb

      emotion = if (world.random.nextDouble() <= aggProb) Emotion.aggressive else Emotion.fleeing
    }
  }
}
