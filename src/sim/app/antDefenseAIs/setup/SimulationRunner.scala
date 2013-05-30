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
import sim.display.Console

object SimulationRunner {

  /**
   * Experiment which will be executed
   */
  private var experiment: Option[Experiment] = None

  /**
   * True if the experiment should be run with graphical user interface
   */
  private var withGUI: Boolean = true

  /**
   * Object handling definition, description and parsing of options.
   */
  private object Options extends Enumeration {
    val nox = Value("--nox")
    val sim1vs1 = Value("--1vs1")
    val normalOnMulti = Value("--normalOnMulti")
    val modOnMulti = Value("--modifiedOnMulti")

    /**
     * Describes options
     *
     * @param option Option to receive the description for
     * @return Description of the given option
     */
    def describe(option: this.Value): String = {
      option match {
        case `nox` => "Run experiment without user interface (for commandline use)"
        case `sim1vs1` => "Duel between one normal tribe and one modified tribe"
        case `normalOnMulti` => "Simulation of a normal tribe among many other normal tribes"
        case `modOnMulti` => "Simulation of a modyfied tribe among many other normal tribes"
      }
    }

    def parse(s: String): Option[this.Value] = {
      import scala.collection.immutable.HashMap

      val parserMap = {
        var map = HashMap[String, this.Value]()

        for (option <- Options.values)
          map = map.+((option.toString, option))

        map
      }

      parserMap get s
    }
  }

  /**
   * Helpmessage for usage of this program
   */
  val helpMessage: String = {
    "Usage:\n" +
      "Option " + Options.nox.toString + "\t" + Options.describe(Options.nox) + "\n" +
      "Option " + Options.sim1vs1.toString + "\t" + Options.describe(Options.sim1vs1) + "\n" +
      "Option " + Options.normalOnMulti.toString + "\t" + Options.describe(Options.normalOnMulti) + "\n" +
      "Option " + Options.modOnMulti.toString + "\t" + Options.describe(Options.modOnMulti) + "\n"
  }

  def main(args: Array[String]) {
    var allArgumentsValid: Boolean = true

    // Optionhandling
    for (option <- args) {
      val opt = Options.parse(option)

      if (opt == None) {
        allArgumentsValid = false
        println("Invalid option: " + option)
      }
      else {
        opt.get match {
          case Options.nox => withGUI = false
          case Options.sim1vs1 => experiment = Some(new Setup_1vs1(System.currentTimeMillis()))
          case Options.normalOnMulti => experiment = Some(new MultiTribeSetup1(System.currentTimeMillis()))
          case Options.modOnMulti => println("WARNING: Already not defined in class " + this.getClass.getCanonicalName)
        }
      }
    }

    if (experiment.isEmpty) {
      println("No experiment type defined. Please choose a valid option for the experiment.")
      println(helpMessage)
      System.exit(1)
    }
    else if (allArgumentsValid) { // Start Simulation with desired configuration
      if (withGUI) {
        val video: ExperimentGUI = new ExperimentGUI(experiment.get)
        val console: Console = new Console(video)
        console.setVisible(true)
      }
      else {
        SimState.doLoop(experiment.get.getClass, args)
        println(experiment.get.giveReport())
        System.exit(0)
      }
    }
    else {
      println(helpMessage)
      System.exit(1)
    }
  }
}
