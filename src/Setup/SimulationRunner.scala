package Setup

//import sim.engine.SimState
import sim.display.Console

/**
 * User interface
 *
 * Copyright : (c) Jörg D. Weisbarth
 * Date: 05/01/13
 * BSD-3
 */
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
