/*
 * Copyright © 2012 - 2013 by Jörg D. Weisbarth <joerg.bretten@web.de>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License 3 as published by
 * the Free Software Foundation;
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY.
 *
 * See the License.txt file for more details.
 */
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
