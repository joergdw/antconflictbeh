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


trait ConflictBehaviour extends Ant with PheroSystem with EconomicBehaviour {

  /**
   * True iff at least one enemy is in range
   *
   * @return True iff at least one enemy is in range
   */
  protected[this] def enemyClose(): Boolean = {
    val ants = world.antsInNeighbourhoodOf(pos = currentPos, range = 1)

    ants.count(a => a.tribeID != this.tribeID) >= 1
  }

  /**
   * The ant tries to pursuit and to hit ants of strange colonies.
   *
   * If an foreign ant is on own field, it will be hit. If there are no foreign ants on the own field but on an
   * neighbour field instead, one of them will be hit, preferably in the direction the ant went the last step.
   * If there are no enemies around, an exception will be thrown.
   */
  protected[this] def attackNearEnemy() {
    val foreignAntsOnOwnField = world.antsOn(currentPos).filter(a => a.tribeID != tribeID)
    if (foreignAntsOnOwnField.size > 0) // Hit ant preferably on own field
      hit(foreignAntsOnOwnField.head)

    else {
      def directionContainsEnemy(dir: Direction.Value): Boolean = {
        val destiny = Direction.inDirection(currentPos, dir)
        val foreignAntsInDirection = world.antsOn(destiny).filter(a => a.tribeID != tribeID)
        foreignAntsInDirection.size > 0
      }

      val dirs = validDirections
      val directionsContainingEnemies = dirs.filter(directionContainsEnemy)

      if (directionsContainingEnemies.size > 0) {
        def directionSorter(dir: Direction.Value) = Direction.directionDistance(lastDirection, dir)

        val attackedField = Direction.inDirection(currentPos, directionsContainingEnemies.sortBy(directionSorter).head)
        val foreignAntsOnAttackedField = world.antsOn(attackedField).filter(a => a.tribeID != tribeID)
        hit(foreignAntsOnAttackedField.head)

      } else
        throw new IllegalStateException("No foreign colony ant around which can be hit")
    }
  }
}


trait CooldownConflictBehaviour extends ConflictBehaviour {
  val emotionalDwellTime: Int /** How long an individual stays in an individual state before going to another */

  /**
   * Possible emotional states of an ant
   */
  protected[this] object Emotion extends Enumeration {
    val aggressive = Value("Aggressive")
    val defensive = Value("Defensive") /** Ant flees into direction of its home  */

    /** Ant ignores ants of stranger colonies and changes emotional state if it receives a hit  */
    val normal = Value("Normal")

    /** Ant changes emotional state as soon as it sees ants of stranger colonies or receives a hit */
    val undecided = Value("Undecided")
  }

  protected[this] var emotion: Emotion.Value = Emotion.undecided /** Current emotional state */
  protected[this] var nextEmotionChange = emotionalDwellTime /** Time until the next state relaxation */

  /**
   * Cools down the emotional state of the ant until `undecided`
   */
  protected[this] def relax() {
    if (nextEmotionChange <= 0)
      emotion match {
        case Emotion.aggressive => emotion = Emotion.normal; nextEmotionChange = emotionalDwellTime
        case Emotion.defensive => emotion = Emotion.normal; nextEmotionChange = emotionalDwellTime
        case Emotion.normal => emotion = Emotion.undecided
        case Emotion.undecided => // do nothing
      }

    else
      nextEmotionChange -= 1
  }
}
