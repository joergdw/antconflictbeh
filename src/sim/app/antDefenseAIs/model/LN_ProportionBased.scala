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
 * Behaviour configuration of an proportion based Lasius Niger ant
 *
 * @param lowerLimitNormal Beginning with that relationship between friendly ants and enemy ants the ant
 *                         won't flee.
 * @param upperLimitNormal Beginning with that relationship between friendly ants and enemy ants the ant
 *                         will fight.
 */
private[antDefenseAIs] class LN_PB_BehaviourConf(
  val lowerLimitNormal: Double = 0.7,
  val upperLimitNormal: Double = 1.2,
  override val emotionalDwellTime: Int = 8,
  override val notBored: Int = 500,
  override val alpha: Double = 0.98d,
  override val explorationRate: Double = 0.3d,
  override val gamma: Double = 0.98d)
  extends LN_BehaviourConf(emotionalDwellTime, notBored, alpha, explorationRate, gamma)


private[antDefenseAIs] class LN_PB_Generator(
  override val behaviourConf: LN_PB_BehaviourConf) extends AntGenerator {

  /**
   * Creates an NormalAntWorker
   *
   * @param tribeID Tribe the ant belongs to
   * @param world World the ant lives on
   * @return NormalAntWorker
   */
  def apply(tribeID: Int, world: World) = new LN_ProportionBased(tribeID, world, behaviourConf)

  def apply(ant: Ant) =
    behaviourConf match {
      case c: LN_PB_BehaviourConf => new LN_ProportionBased(ant, behaviourConf)
    }
}

private[antDefenseAIs] class LN_ProportionBased(
  override val tribeID: Int,
  override val world: World,
  override val behaviourConf: LN_PB_BehaviourConf)
  extends LasiusNiger(tribeID, world, behaviourConf) {

  import behaviourConf.{lowerLimitNormal, upperLimitNormal}

  /**
   * Constructs ant with the information of the given ant
   *
   * @param ant Ant ant giving the information of construction
   * @return Ant of the same colony in the same simulation
   */
  def this(ant: Ant, behaviourConf: LN_PB_BehaviourConf) = this(ant.tribeID, ant.world, behaviourConf)

  override def adaptState() {
    evalueSituation() match {
      case None                                 => // Do nothing
      case Some(rel) if rel < lowerLimitNormal  => emotion = Emotion.fleeing
      case Some(rel) if rel <= upperLimitNormal => emotion = Emotion.normal
      case Some(rel)                            => emotion = Emotion.aggressive
    }

    nextEmotionChange = emotionalDwellTime
  }

  override def receiveHitFrom(opponent: Ant) {
    super.receiveHitFrom(opponent)

    val limit: Double = (lowerLimitNormal + upperLimitNormal) / 2
    evalueSituation() match {
      case None                      => // Do nothing
      case Some(rel) if rel < limit  => emotion = Emotion.fleeing
      case Some(rel)                 => emotion = Emotion.aggressive
    }
  }
}
