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
   * Stops the simulation as soon as the stopping criteria defined with `experimentShouldBeStopped` is fullfilled.
   *
   * @param state Parameter not used
   */
  override def step(state: SimState) {
    if (experimentShouldBeStopped()) {

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
  def experimentShouldBeStopped(): Boolean

  /**
   * Actions performed at the end of an experiment
   */
  override def finish() = println(giveReport())

  /**
   * Report of the current state of the experiment
   *
   * @return Report message of the current state of the experiment
   */
  def giveReport(): String = {
    def header() = {
      import java.util.Date
      import java.sql.Timestamp

      "Experiment Report\n--------------------\n" +
      "Current System Time: " + new Timestamp(new Date().getTime).toString + "\n\n"
    }

    def populationReport(): String = {
      val population: List[(Int, Int)] = world.populationStat().toList.sortBy(x => x._1)
      var message = "* Population overview:\n"

      for ((id, number) <- population) {
        message = message concat "\tTribe " + id + " has " + number + " ants\n"
      }

     message concat "\n"
    }

    def resourceReport(): String = {
      val resourcesTotal: List[(Int, Int)] = world.totalResStat().toList.sortBy(x => x._1)
      val resourcesQueens = world.resourceStat()
      var message: String = "* Resource overview:\n"

      for ((id, amount) <- resourcesTotal) {
        message = message concat "\tTribe " + id + " owns " + amount + " resources"
        message = message concat ", " + resourcesQueens(id) + " has the queen and"
        message = message concat " " + (amount - resourcesQueens(id)) + " are carried by the workers."
        message = message concat "\n"
      }

      message concat "\n"
    }

    def lossesReport(): String = {
      val totalLosses = world.lostAntsByTribe().toList.sortBy(x => x._1)
      val lostByAge = world.lostAntsByAge()
      var message: String = "* Losses overview:\n"

      for ((id, total) <- totalLosses) {
        message = message concat "\tTribe " + id + " suffered " + total + " losses"
        message = message concat ", " + lostByAge(id) + " of them by age and"
        message = message concat " " + (total - lostByAge(id)) + " due to enemy contact."
        message = message concat "\n"
      }

      message concat "\n"
    }

    def queensReport(): String = {
      val survivedState = world.queensSurvived().toList.sortBy(x => x._1)
      var message: String = "* Queens status report:\n"

      for((id, survived) <- survivedState) {
        message = message concat "\tThe queen of tribe " + id + " survived = " + survived + "\n"
      }

      message concat "\n"
    }

    header + populationReport() + resourceReport() + lossesReport() + queensReport()
  }
}