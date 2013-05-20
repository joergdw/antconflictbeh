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
import scala.collection.immutable.HashMap

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

  // TODO: Automatische Abbruch-Kriterien entwickeln und beachten.

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

    header + populationReport() + resourceReport() + lossesReport()
  }
}