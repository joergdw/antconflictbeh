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

import sim.engine.SimState

import sim.app.antDefenseAIs.model.World

/**
 * Contains some preferences for the simulation
 *
 * @param s Seed for random data generator
 */
abstract class Experiment(var s: Long) extends SimState(s) {

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
    world.start()
  }

  /**
   * Report of the current state of the experiment
   *
   * @return Report message of the current state of the experiment
   */
  def getReport(): String = {
    def header() = {
      import java.util.Date
      import java.sql.Timestamp

      "Experiment Report\n--------------------\n" +
      "Current System Time: " + new Timestamp(new Date().getTime).toString + "\n\n"
    }

    def populationReport(): String = {
      val population: Array[Int] = world.populationStat()
      var message = "* Population overview:\n"

      for (i <- 0 until numberOfTribes) {
        message = message concat "\tTribe " + i + " has " + population(i) + " ants\n"
      }

     message concat "\n"
    }

    def resourceReport(): String = {
      val resourcesTotal: Array[Int] = world.totalResStat()
      val resourcesQueens: Array[Int] = world.resourceStat()
      var message: String = "* Resource overview:\n"

      for (i <- 0 until numberOfTribes) {
        message = message concat "\tTribe " + i + " owns " + resourcesTotal(i) + " resources"
        message = message concat ", " + resourcesQueens(i) + " has the queen and"
        message = message concat " " + (resourcesTotal(i) - resourcesQueens(i)) + " are carried by the workers."
        message = message concat "\n"
      }

      message concat "\n"
    }

    def lossesReport(): String = {
      val totalLosses = world.lostAntsByTribe()
      val lostByAge = world.lostAntsByAge()
      var message: String = "* Losses overview:\n"

      for (i <- 0 until numberOfTribes) {
        message = message concat "\tTribe " + i + " suffered " + totalLosses(i) + " losses"
        message = message concat ", " + lostByAge(i) + " of them by age and"
        message = message concat " " + (totalLosses(i) - lostByAge(i)) + " due to enemy contact."
        message = message concat "\n"
      }

      message concat "\n"
    }

    header + populationReport() + resourceReport() + lossesReport()
  }
}