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

  def isAggressive = emotion > neutralUpperBound
  def isDefensive = emotion < neutralLowerBound
  def isNeutral = !(isAggressive || isDefensive)

  /**
   * As of this value of other ants of the same colony, the ant changes state with probability
   * `maxAggressivenessProb`.
   */
  val maxAgressiveness = 40

  val maxAggressivenessProb = 0.767d /** Highest possible probability that an ant gets aggressive */
  val minAggressivenessProb = 0.257d /** Lowest possible probability that an ant gets aggressive */

  val antsSensingRange: Int = 4 /** Radius of the area the ant can sense other individuals */


  ///////////////////// Basic operations /////////////////////////////////////

  override def receiveHit(opponent: AntWorker) {
    super.receiveHit(opponent)
    if (this.isDead) return // Ant dead: no more actions

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
    val alpha = countFriends() / maxAgressiveness
    val aggressivenessProb = alpha * maxAggressivenessProb + (1 - alpha) * minAggressivenessProb
    emotion = if (world.random.nextDouble() <= aggressivenessProb) aggressive else defensive
  }

  /**
   * Counts the number of ants of the same colony within the neighbourhood.
   *
   * The size of the observed neighbourhood is indicated by `antsSensingRange`.
   *
   * @return Number of ants of the same colony in the neighbourhood
   */
  def countFriends(): Int = countAntsFullfillingPredicate(a => a.tribeID == this.tribeID)

  /**
   * Counts the number of ants of other colonies within the neighbourhood.
   *
   * The size of the observed neighbourhood is indicated by `antsSensingRange`.
   *
   * @return Number of ants of other colonies in the neighbourhood
   */
  def countStrangers(): Int = countAntsFullfillingPredicate((a: Ant) => a.tribeID != this.tribeID)

  /**
   * Counts the number of ants within the neighbourhood fulfilling a predicate.
   *
   * The size of the observed neighbourhood is indicated by `antsSensingRange`.
   *
   * @param p Predicate
   * @return Number of ants in the neighbourhood fulfilling the predicate p
   */
  def countAntsFullfillingPredicate(p: Ant => Boolean): Int = {
    val ants: List[Ant] = neighbourhood(antsSensingRange).map(world.antsOn).flatten
    def adder(i: Int, a: Ant): Int = i + (if (p(a)) 1 else 0)
    ants.foldLeft(0: Int)(adder)
  }


  ///////////////////// Behaviour description /////////////////////////////////////

  override def step(state: SimState) {

    /*
     If the ant is deeply neutral (i.e. emotion == 0) it adapts its state when there are more than
     `threshold_strangers` ants of other colonies and more than `threshold_friends` ants of the own
     colony in the neighbourhood. The first condition ensures that the ant does not change every simulation step its
     behaviour.
      */
    val threshold_strangers = 1
    val threshold_friends = 10
    if (emotion == 0 && countStrangers() >= threshold_strangers && countFriends() >= threshold_friends)
      adaptState()

    if (isNeutral) actEconomically(state)
    else if (isAggressive) actMilitarily()
    else followHomeWay() //; assert(isDefensive)
  }

  /**
   * The ant tries to pursuit and to hit ants of strange colonies.
   */
  override def actMilitarily() {
    // TODO
  }
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