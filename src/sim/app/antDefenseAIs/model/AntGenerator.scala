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
package sim.app.antDefenseAIs.model

/**
 * Generator for ants
 */
private[antDefenseAIs] trait AntGenerator {

  val behaviourConf: BehaviourConf /** Configuration used by this generator to generate new ant workers */

  /**
   * Constructs a new ant
   *
   * @param ant Ant of the same tribe in the same simulation (world) like the new one
   * @param workerConf Configuration used for the ant ant
   *
   * @return Ant ant of the same tribe in the same simulation (world) like the given one
   */
  def apply(ant: Ant): AntWorker
}
