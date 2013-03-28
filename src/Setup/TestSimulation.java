package Setup;

import sim.engine.SimState;

/**
 * TODO: Add file description
 *
 * User: kairos
 * Date: 5/01/13
 * License: TODO: Add license
 */
public class TestSimulation extends SimState {

    public TestSimulation(long seed) {
        super(seed);
    }

    public static void main(String[] args) {
        {
            doLoop(TestSimulation.class, args);
            System.exit(0);
        }
    }
}
