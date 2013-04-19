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
 * Generator for unique tribe ids
 */
private object TribeIDGenerator {
  private var IDOfLastTribe: Int = -1 /** ID of the last generated tribe */  // -1 so that 0 is the ID of the first tribe

  /**
   * Generates a unique tribe id
   *
   * @return Unique tribe id
   */
  def nextTribeID(): Int = {
    IDOfLastTribe += 1
    IDOfLastTribe
  }
}
