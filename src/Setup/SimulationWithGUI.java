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

import sim.display.Controller;
import sim.display.Display2D;
import sim.display.GUIState;
import sim.engine.SimState;
import sim.engine.Steppable;
import sim.portrayal.DrawInfo2D;
import sim.portrayal.grid.FastValueGridPortrayal2D;
import sim.portrayal.grid.SparseGridPortrayal2D;
import sim.portrayal.simple.OvalPortrayal2D;
import sim.util.gui.SimpleColorMap;

import javax.swing.*;
import java.awt.*;
import java.util.Arrays;

public class SimulationWithGUI extends GUIState {
    SparseGridPortrayal2D ants = new SparseGridPortrayal2D();
    FastValueGridPortrayal2D resources = new FastValueGridPortrayal2D();
    public Display2D antDisplay, resDisplay;
    public JFrame antDisplayFrame, resDisplayFrame;

    // Further arrays for all the pheromone-displays
    FastValueGridPortrayal2D[] homePheros, resPheros, warPheros;
    public Display2D[] homePheroDisplays, resPheroDisplays, warPheroDisplays;
    public JFrame[] homePheroDisplaysFrame, resPheroDisplaysFrame, warPheroDisplaysFrame;

    // Statistical displays
    org.jfree.data.xy.XYSeries series;    // the data series we'll add to
    sim.util.media.chart.TimeSeriesChartGenerator chart;  // the charting facility
    JFrame chartFrame;

    // Colour-map for the differen tribes
    private Color[] tribeColours = {Color.black, Color.blue, Color.red,
                                    Color.yellow, Color.cyan, Color.green,
                                    Color.orange, Color.lightGray};

    public static String getName() { return "Ant wars";}


////////// Constructors and constructor related helpers ////////////////////////////////////// ////////////////
    public SimulationWithGUI() { super(new Simulation(System.currentTimeMillis())); initialiseDisplays();}
    public SimulationWithGUI(SimState state) { super(state); initialiseDisplays();}

    private void initialiseDisplays() {
        int t = ((Simulation) state).nTribes();
        homePheroDisplays = new Display2D[t];
        resPheroDisplays = new Display2D[t];
        warPheroDisplays = new Display2D[t];

        homePheroDisplaysFrame = new JFrame[t];
        resPheroDisplaysFrame = new JFrame[t];
        warPheroDisplaysFrame = new JFrame[t];

        homePheros = new FastValueGridPortrayal2D[t];
        resPheros = new FastValueGridPortrayal2D[t];
        warPheros = new FastValueGridPortrayal2D[t];

        for (int i = 0; i < homePheros.length; ++i) {
            homePheros[i] = new FastValueGridPortrayal2D();
            resPheros[i] = new FastValueGridPortrayal2D();
            warPheros[i] = new FastValueGridPortrayal2D();
        }
    }


///////////////////////////// Stuff to start- and load the simulation //////////////////////////////////

    @Override
    public void start() {
      super.start();
      setupPortrayals();
    }

    @Override
    public void load(SimState state) {
        super.load(state);
        setupPortrayals();
    }

    private void setupPortrayals() {
        setupAntPortrayal();
        setupResourcePortrayal();
        setupPheromonePortrayals();
//        setupChartPortrayal(); // TODO: Debug method befor including it (see other todo-comment)
    }

    private void setupAntPortrayal() {
        final Simulation simulation = (Simulation) state;

        // tell the portrayals what to portray and how to portray them
        ants.setField(simulation.ants());
        ants.setPortrayalForAll(new OvalPortrayal2D() {
            public void draw(Object object, Graphics2D graphics, DrawInfo2D info) {
                Ant ant = (Ant) object;
                paint = tribeColours[ant.tribe().tribeID()];
                super.draw(object, graphics, info);
            }
        });


        antDisplay.reset(); // reschedule the displayer
        antDisplay.setBackdrop(Color.white);
        antDisplay.repaint(); // redraw the antDisplay
    }

    private void setupResourcePortrayal() {
        final Simulation simulation = (Simulation) state;

        // Portrayal for the resource-map
        resources.setMap(new SimpleColorMap(0, simulation.maxResAmount(), Color.white, Color.black));
        resources.setField(simulation.resources());
        resources.setValueName("Amount resources");
        resources.setPortrayalForAll(new OvalPortrayal2D());

        resDisplay.reset();
        resDisplay.setBackdrop(Color.white);
        resDisplay.repaint();
    }

    private void setupPheromonePortrayals() {
        final Simulation simulation = (Simulation) state;

        // Portrayals for the different Pheromone-Grids
        for (int i = 0; i < homePheros.length; ++i) {

            homePheros[i].setField(simulation.tribes()[i].homePhero());
            resPheros[i].setField(simulation.tribes()[i].resPhero());
            warPheros[i].setField(simulation.tribes()[i].warPhero());

            homePheros[i].setMap(new SimpleColorMap(0, Integer.MAX_VALUE, Color.white, Color.black));
            resPheros[i].setMap(new SimpleColorMap(0.0d, 1.0d, Color.white, Color.black));
            warPheros[i].setMap(new SimpleColorMap(0.0d, 1.0d, Color.white, Color.black));

            homePheros[i].setValueName("Home pheromones map of tribe " + i);
            resPheros[i].setValueName("Resource pheromones map of tribe " + i);
            warPheros[i].setValueName("War pheromones map of tribe " + i);

            homePheros[i].setPortrayalForAll(new OvalPortrayal2D());
            resPheros[i].setPortrayalForAll(new OvalPortrayal2D());
            warPheros[i].setPortrayalForAll(new OvalPortrayal2D());
        }


        for (int i = 0; i < homePheroDisplays.length; ++i) {
            homePheroDisplays[i].reset();
            resPheroDisplays[i].reset();
            warPheroDisplays[i].reset();

            homePheroDisplays[i].setBackdrop(Color.white);
            resPheroDisplays[i].setBackdrop(Color.white);
            warPheroDisplays[i].setBackdrop(Color.white);

            homePheroDisplays[i].repaint();
            resPheroDisplays[i].repaint();
            warPheroDisplays[i].repaint();
        }
    }

    private void setupChartPortrayal() {
        // Chart setup
        chart.removeAllSeries();
        series = new org.jfree.data.xy.XYSeries(
                "Put a unique name for this series here so JFreeChart can hash with it",
                false);
        chart.addSeries(series, null);
        scheduleRepeatingImmediatelyAfter(new Steppable()
        {
            public void step(SimState state)
            {
                // at this stage we're adding data to our chart.  We
                // need an X value and a Y value.  Typically the X
                // value is the schedule's timestamp.  The Y value
                // is whatever data you're extracting from your
                // simulation.  For purposes of illustration, let's
                // extract the number of steps from the schedule and
                // run it through a sin wave.

                double x = state.schedule.time();
                double y = Math.sin(state.schedule.getSteps()) * 10;

                // now add the data
                if (x >= state.schedule.EPOCH && x < state.schedule.AFTER_SIMULATION)
                {
                    series.add(x, y, true);

                    // we're in the model thread right now, so we shouldn't directly
                    // update the chart.  Instead we request an update to occur the next
                    // time that control passes back to the Swing event thread.
                    chart.updateChartLater(state.schedule.getSteps());
                }
            }
        });
    }


////////////       ///////////////////////////////////

    public void init(Controller c) {
        super.init(c);

        // antDisplay
        antDisplay = new Display2D(500,500,this);
        antDisplay.setClipping(false);
        antDisplayFrame = antDisplay.createFrame();
        antDisplayFrame.setTitle("Ants Display");
        c.registerFrame(antDisplayFrame);

        antDisplayFrame.setVisible(true);
        antDisplay.attach(ants, "Ant World"); // so the frame appears in the "Display" list

        // resDisplay
        resDisplay = new Display2D(500, 500, this);
        resDisplay.setClipping(false);
        resDisplayFrame = resDisplay.createFrame();
        resDisplayFrame.setTitle("Resources");
        c.registerFrame(resDisplayFrame);

        resDisplayFrame.setVisible(true);
        resDisplay.attach(resources, "Resources");

        // Phero-displays
        for (int i = 0; i < homePheroDisplays.length; ++i) {
            homePheroDisplays[i] = new Display2D(500, 500, this);
            resPheroDisplays[i] = new Display2D(500, 500, this);
            warPheroDisplays[i] = new Display2D(500, 500, this);

            homePheroDisplays[i].setClipping(false);
            resPheroDisplays[i].setClipping(false);
            warPheroDisplays[i].setClipping(false);

            homePheroDisplaysFrame[i] = homePheroDisplays[i].createFrame();
            resPheroDisplaysFrame[i] = resPheroDisplays[i].createFrame();
            warPheroDisplaysFrame[i] = warPheroDisplays[i].createFrame();

            homePheroDisplaysFrame[i].setTitle("Home pheromones of tribe " + i);
            resPheroDisplaysFrame[i].setTitle("Resource pheromones of tribe " + i);
            warPheroDisplaysFrame[i].setTitle("War pheromones of tribe " + i);

            c.registerFrame(homePheroDisplaysFrame[i]);
            c.registerFrame(resPheroDisplaysFrame[i]);
            c.registerFrame(warPheroDisplaysFrame[i]);

//            homePheroDisplaysFrame[i].setVisible(true);
//            resPheroDisplaysFrame[i].setVisible(true);
//            warPheroDisplaysFrame[i].setVisible(true);

            homePheroDisplays[i].attach(homePheros[i], "Home pheromones of tribe " + i);
            resPheroDisplays[i].attach(resPheros[i], "Resource pheromones of tribe " + i);
            warPheroDisplays[i].attach(warPheros[i], "War pheromones of tribe " + i);
        }

        // Chart display
        chart = new sim.util.media.chart.TimeSeriesChartGenerator();           // TODO: Problematic instruction. Execution of method stops here
        chart.setTitle("Population statistics");
        chart.setRangeAxisLabel("Put the name of your charted series here");
        chart.setDomainAxisLabel("Time");
        chartFrame = chart.createFrame();
        // perhaps you might move the chart to where you like.
        chartFrame.setVisible(true);
        chartFrame.pack();
        c.registerFrame(chartFrame);

        // the console automatically moves itself to the right of all
        // of its registered frames -- you might wish to rearrange the
        // location of all the windows, including the console, at this
        // point in time....
    }

    public void quit() {
        super.quit();

        Simulation sim = (Simulation) state;
        System.out.println(Arrays.toString(sim.populationStat()));
        System.out.println(Arrays.toString(sim.resourceStat()));
        System.out.println(Arrays.toString(sim.totalResStat()));
        System.out.println(Arrays.deepToString(sim.resourceMap()));

        if (antDisplayFrame !=null) antDisplayFrame.dispose();
        antDisplayFrame = null;
        antDisplay = null;

        if (resDisplayFrame != null) resDisplayFrame.dispose();
        resDisplayFrame = null;
        resDisplay = null;
    }
}
