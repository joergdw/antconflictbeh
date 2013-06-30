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
 * Behaviour configuration of an normal Lasius Niger ant
 *
 * @param maxAggressiveness As of this value for the relationship between other ants of the same colony and enemy-ants
 *                          , the ant changes state with probability `maxAggressivenessProb`.
 * @param minNeutralnessProb Lowest possible probability that an ant ignores others
 * @param maxNeutralnessProb Highest possible probability that an ant ignores others
 * @param minAggressivenessProb Lowest possible probability that an ant gets aggressive
 * @param maxAggressivenessProb Highest possible probability that an ant gets aggressive
 */
private[antDefenseAIs] class LN_RPB_WF_BehaviourConf(
  val shoutingRange: Int = 3,
  override val maxAggressiveness: Double = 6d,
  override val minNeutralnessProb: Double = 0.33d,
  override val maxNeutralnessProb: Double = 0.2d,
  override val minAggressivenessProb: Double = 0.257d,
  override val maxAggressivenessProb: Double = 0.767d,
  override val emotionalDwellTime: Int = 8,
  override val notBored: Int = 500,
  override val alpha: Double = 0.98d,
  override val explorationRate: Double = 0.3d,
  override val gamma: Double = 0.98d)
  extends LN_RPB_BehaviourConf(maxAggressiveness, minNeutralnessProb, maxNeutralnessProb, minAggressivenessProb,
    maxAggressivenessProb, emotionalDwellTime, notBored, alpha, explorationRate, gamma)


private[antDefenseAIs] class LN_RPB_WF_Generator(
  override val behaviourConf: LN_RPB_WF_BehaviourConf) extends AntGenerator {

  /**
   * Creates an NormalAntWorker
   *
   * @param tribeID Tribe the ant belongs to
   * @param world World the ant lives on
   * @return NormalAntWorker
   */
  def apply(tribeID: Int, world: World) = new LN_RPB_WithFetching(tribeID, world, behaviourConf)

  def apply(ant: Ant) =
    behaviourConf match {
      case c: LN_RPB_WF_BehaviourConf => new LN_RPB_WithFetching(ant, behaviourConf)
    }
}

private[antDefenseAIs] class LN_RPB_WithFetching(
  override val tribeID: Int,
  override val world: World,
  override val behaviourConf: LN_RPB_WF_BehaviourConf)
  extends LN_RandomPB(tribeID, world, behaviourConf) {
  // Initialise configuration
  import behaviourConf._


  /**
   * Constructs ant with the information of the given ant
   *
   * @param ant Ant ant giving the information of construction
   * @return Ant of the same colony in the same simulation
   */
  def this(ant: Ant, behaviourConf: LN_RPB_WF_BehaviourConf) = this(ant.tribeID, ant.world, behaviourConf)

  protected[this] def followWarPheromone() {
    val destiny = chooseDirectionBy(warPheroOf)

    if (destiny.isDefined) {
      moveTo(destiny.get)
      adaptAllPheros()
    }

    mineRes()
  }

  override protected[model] def actEconomically() {
    val bestWarDir = gradientOf(warPheroOf)
    val bestWarPhero = warPheroOf(bestWarDir)

    if (bestWarPhero > 0)
      followWarPheromone()
    else
      super.actEconomically()
  }

  // Overrided to set war pheromone
  override def adaptState() {
    import StrictMath.min

    val alpha:Double = min(1d, evalueSituation().get / maxAggressiveness)
    val aggressivenessProb = alpha * maxAggressivenessProb + (1 - alpha) * minAggressivenessProb
    val neutralnessProb = alpha * maxNeutralnessProb + (1 - alpha) * minNeutralnessProb
    assert (aggressivenessProb + neutralnessProb <= 1)

    val random = world.random.nextDouble()
    emotion = if (random <= aggressivenessProb)
                Emotion.aggressive
              else if (random <= aggressivenessProb + neutralnessProb) {
                shout() // Shout to create a numerical superiority
                Emotion.normal
              }
              else
                Emotion.fleeing

    nextEmotionChange = emotionalDwellTime
  }

  /**
   * Gives a shout of war pheromones
   */
  private[this] def shout() {
    import sim.app.antDefenseAIs.maxDistance

    val curPos: (Int, Int) = currentPos
    val positions: List[(Int, Int)] = world.nearPos(this, shoutingRange).get

    def phero(p: (Int, Int)): ((Int, Int), Double) = (p, 1d / (maxDistance(p, curPos).toDouble + 1d))

    for ((pos, value) <- positions.map(phero)) {
      world.setWarPheroOn(this, pos, value)
    }
  }
}
