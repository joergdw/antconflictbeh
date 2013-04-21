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
package sim.app.antDefenseAIs.setup

//import sim.engine.SimState
import sim.display.Console

object SimulationRunner {

  def main(args: Array[String]) {
    //val dummy: Simulation = new Simulation(14l) // only needed to get access to the class
    //SimState.doLoop(dummy.getClass, args)
    //System.exit(0)

    val video: SimulationWithGUI = new SimulationWithGUI
    val console: Console = new Console(video)
    console.setVisible(true)
  }
}
