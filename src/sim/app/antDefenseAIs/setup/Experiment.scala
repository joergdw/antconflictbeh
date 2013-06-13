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
package sim.app.antDefenseAIs.setup

import sim.engine.{Steppable, SimState}

import sim.app.antDefenseAIs.model.World

/**
 * Contains some preferences for the simulation
 *
 * @param s Seed for random data generator
 */
abstract class Experiment(var s: Long) extends SimState(s) with Steppable {

  val name: String = getClass.getName

  val world: World

  /**
   * Maximum number of resources on a field on the world
   *
   * @return Maximum number of resources on a field on the world
   */
  def maxResAmount = world.maxResAmount

  val numberOfTribes: Int /** Number of tribes on this world */

  override def start() {
    super.start()
    schedule.scheduleRepeating(this)
    world.start()
  }

  /**
   * Stops the simulation as soon as the stopping criteria defined with `stopCriteriaFulfilled` is fulfilled.
   *
   * @param state Parameter not used
   */
  override def step(state: SimState) {
    if (stopCriteriaFulfilled()) {

      schedule.scheduleOnceIn(0,
        new Steppable() {
          override def step(state: SimState) {
            state.kill()
            assert (schedule.scheduleComplete())
            println("Experiment done, empty scheduling queue.")
          }
        })
    }
  }

  /**
   * True if the experiment should be stopped.
   *
   * @return True if the experiment should be stopped
   */
  def stopCriteriaFulfilled(): Boolean

  /**
   * Actions performed at the end of an experiment
   */
  override def finish() {
    println(giveReport())
  }

  /**
   * Report of the current state of the experiment
   *
   * @return Report message of the current state of the experiment
   */
  def giveReport(): String = {
    val colInfs: List[(Int, world.ColonyInfo)] = world.colonyInfos.toList.sortBy(x => x._1) // sorted by tribeID

    val header = {
      import java.util.Date
      import java.sql.Timestamp

      "Experiment Report\n--------------------\n" +
      "Current System Time: " + new Timestamp(new Date().getTime).toString + "\n" +
      "Experiment name: " + name + "\n\n"
    }

    var report = ""

      for ((id, cInf) <- colInfs) {
        report = report concat
         "* Overview of colony " + id + "\n" +
         "\tInitial start position:\t " + cInf.initialStartPosition + "\n" +
         "\tCurrent population:\t " + cInf.population() + "\n" +
         "\tAmount of owned resources:\t " + cInf.resources() + " (" + cInf.deposit() + " of them has the queen)" + "\n" +
         "\tSuffered losses:\t" + (cInf.beingKilled + cInf.deceased) + " (" + cInf.deceased + " of them due to age)" + "\n" +
         "\tQueen survived:\t " + cInf.queenSurvived() + "\n" +
         "\tGiven hits in mayority situations:\t" + cInf.analyzeTable(cInf.hit_made)(cInf.sit_mayority) + "\n" +
         "\tGiven hits in equal situations:\t" + cInf.analyzeTable(cInf.hit_made)(cInf.sit_equal) + "\n" +
         "\tGiven hits in minority situations:\t" + cInf.analyzeTable(cInf.hit_made)(cInf.sit_minority) + "\n" +
         "\tReceived hits in mayority situations:\t" + cInf.analyzeTable(cInf.hit_gotten)(cInf.sit_mayority) + "\n" +
         "\tReceived hits in equal situations:\t" + cInf.analyzeTable(cInf.hit_gotten)(cInf.sit_equal) + "\n" +
         "\tReceived hits in minority situations:\t" + cInf.analyzeTable(cInf.hit_gotten)(cInf.sit_minority) + "\n\n"
      }

    header + report
  }
}