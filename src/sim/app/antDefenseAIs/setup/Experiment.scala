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
    val colInfs = world.colonyInfos.toList.sortBy(x => x._1) // sorted by tribeID

    def header() = {
      import java.util.Date
      import java.sql.Timestamp

      "Experiment Report\n--------------------\n" +
      "Current System Time: " + new Timestamp(new Date().getTime).toString + "\n" +
      "Experiment name: " + name + "\n\n"
    }

    def populationReport(): String = {
      var message = "* Population overview:\n"

      for ((id, cInf) <- colInfs) {
        val pos = cInf.initialStartPosition
        val number = cInf.population()
        message = message concat "\tTribe " + id + " has " + number + " ants and start position " + pos + "\n"
      }

     message concat "\n"
    }

    def resourceReport(): String = {
      var message: String = "* Resource overview:\n"

      for ((id, cInf) <- colInfs) {
        val (resourcesTotal, queenDeposit) = (cInf.resources(), cInf.deposit())

        message = message concat "\tTribe " + id + " owns " + resourcesTotal + " resources"
        message = message concat ", " + queenDeposit + " has the queen and"
        message = message concat " " + (resourcesTotal - queenDeposit) + " are carried by the workers."
        message = message concat "\n"
      }

      message concat "\n"
    }

    def lossesReport(): String = {
      var message: String = "* Losses overview:\n"

      for ((id, cInf) <- colInfs) {
        val (beingKilled, deceased) = (cInf.beingKilled, cInf.deceased)

        message = message concat "\tTribe " + id + " suffered " + (beingKilled + deceased) + " losses"
        message = message concat ", " + deceased + " of them by age and"
        message = message concat " " + beingKilled + " due to enemy contact."
        message = message concat "\n"
      }

      message concat "\n"
    }

    def queensReport(): String = {
      var message: String = "* Queens status report:\n"

      for((id, cInf) <- colInfs)
        message = message concat "\tThe queen of tribe " + id + " survived = " + cInf.queenSurvived() + "\n"

      message concat "\n"
    }

    header + populationReport() + resourceReport() + lossesReport() + queensReport()
  }
}