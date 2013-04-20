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
package AntDefenseAIs.Model

import sim.engine.SimState
import util.Random

/**
 * Antworker with the usual strategies
 *
 * @param tribeID Tribe the ant belongs to
 * @param world World the ant lives on
 */
private[AntDefenseAIs] class NormalAntWorker(
  override val tribeID: Int,
  override val world: World) extends AntWorker(tribeID, world) {

  /**
   * Constructs ant with the information of the given ant
   *
   * @param ant Ant giving the information of construction
   * @return Ant of the same colony in the same simulation
   */
  def this(ant: Ant) = this(ant.tribeID, ant.world)

  ///////////////////// AntDefenseAIs.Common variables and constants /////////////////////////////////////

  /**
   * Descripes the range of the emotions. Value must be odd and dividable by 3.
   *
   * Value determines the possible values of the variable `emotion`.
   * It's range is in [-(`emotionalRange` div 2), (`emotionalRange` div 2)]
   * `emotion` in the lower third of the scala indicates that the ant is the defensive state,
   * the middle third in the neutral state and the upper third in the aggressive state.
   */
  val emotionalRange: Int = 15
  private var emotion: Int = 0

  val sectionSize:Int = emotionalRange / 3  /** Size of each emotional state on the scala */
  val defensive: Int = - (emotionalRange / 2)
  val aggressive: Int = (emotionalRange / 2)
  val neutralLowerBound: Int = defensive + sectionSize /** Lower bound of the neutral state */
  val neutralUpperBound: Int = aggressive - sectionSize /** Upper bound of the neutral state */

  val maxAgressiveness = 100 /** As of this value the ant changes state with probability `maxAggressivenessProb` */

  val maxAggressivenessProb = 0.767d /** Highest possible probability that an ant gets aggressive */
  val minAggressivenessProb = 0.257d /** Lowest possible probability that an ant gets aggressive */

  val antsSensingRange: Int = 3 /** Radius of the area the ant can sense other individuals */


  ///////////////////// Basic operations /////////////////////////////////////

  override def receiveHit(opponent: AntWorker) {
    super.receiveHit(opponent)
    if (this.hitpoints == 0) return // Ant dead: no more actions

    if (neutralLowerBound <= emotion && emotion <= neutralUpperBound) // If ant neutral…
      adaptState() // … calculate new state
  }

  /**
   * Adapts the emotional state of the ant.
   *
   * Changes either to aggressive state or to defensive state, in function of the number of ants
   * of the same colony in the nearby environment.
   *
   * It is assumed that the chance that an ant gets aggressive grows linearly.
   */
  def adaptState() {
    val r = new Random()

    val alpha = countFriends() / maxAgressiveness
    val aggressivenessProb = alpha * maxAggressivenessProb + (1 - alpha) * minAggressivenessProb
    emotion = if (r.nextDouble() <= aggressivenessProb) aggressive else defensive
  }

  /**
   * Counts the number of ants of the same colony within the neighbourhood.
   *
   * The size of the observed neighbourhood is indicated by `antsSensingRange`.
   *
   * @return Number of ants of the same colony
   */
  def countFriends(): Int = {
    val ants: List[Ant] = nearPos(antsSensingRange).map(world.antsOn).flatten
    def adder(i: Int, a: Ant): Int = i + (if (a.tribeID == this.tribeID) 1 else 0)
    ants.foldLeft(0: Int)(adder)
  }


  ///////////////////// Behaviour description /////////////////////////////////////

  override def step(state: SimState) {
    // TODO: Add more behaviour
    actEconomically(state)
  }

  override def actMilitarily(state: SimState) {}
}

private[AntDefenseAIs] object NormalAntWorker extends AntGenerator {

  /**
   * Creates an NormalAntWorker
   *
   * @param tribeID Tribe the ant belongs to
   * @param world World the ant lives on
   * @return NormalAntWorker
   */
  def apply(tribeID: Int, world: World) = new NormalAntWorker(tribeID, world)

  def apply(ant: Ant) = new NormalAntWorker(ant)
}