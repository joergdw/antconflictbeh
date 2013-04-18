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
package Model

/**
 * Generator for ants
 */
trait AntGenerator {

  /**
   * Constructs a new ant
   *
   * @param ant Ant of the same tribe in the same simulation (world) like the new one
   * @return Ant of the same tribe in the same simulation (world) like the given one
   */
  def apply(ant: Ant): Ant
}
