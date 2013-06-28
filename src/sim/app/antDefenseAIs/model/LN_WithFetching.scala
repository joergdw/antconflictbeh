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
import sim.engine.SimState

class LN_WithFetching(
  override val tribeID: Int,
  override val world: World,
  override val behaviourConf: LN_BehaviourConf)
  extends LasiusNiger(tribeID, world, behaviourConf) {

  import behaviourConf._

  override def adaptState() {
    val alpha = min(1, countFriends() / maxAggressiveness)
    val aggressivenessProb = alpha * maxAggressivenessProb + (1 - alpha) * minAggressivenessProb

    emotion = if (world.random.nextDouble() <= aggressivenessProb)
                Emotion.aggressive
              else {
                setWarPhero(1.0d)
                Emotion.defensive
              }
    nextEmotionChange = emotionalDwellTime
  }

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
      case Emotion.aggressive => {
        if (enemyClose())
          attackNearEnemy()
        else if (gradientOf(warPheroOf).isDefined) {
          moveTo(gradientOf(warPheroOf).get)
          adaptAllPheros()
        }
        else
          actEconomically()
      }
      case Emotion.defensive => followHomeWay()
      case e if e == Emotion.normal || e == Emotion.undecided => actEconomically()
    }

    relax()
  }
}
