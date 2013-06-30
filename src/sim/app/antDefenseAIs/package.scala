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
package sim.app

import StrictMath.{abs, max}

import sim.util.{Bag, IntBag, Int2D}
import sim.field.grid.{DoubleGrid2D, IntGrid2D}

import sim.app.antDefenseAIs.model.Ant

/** Helping functions and constants */
package object antDefenseAIs {

  /**
   * Converts Int2D to (Int, Int)
   */
  implicit def toTuple(i: Int2D): (Int, Int) = (i.getX, i.getY)

  /** Converts (Int, Int) to Int2D */
  implicit def toInd2D(t: (Int, Int)): Int2D = new Int2D(t._1, t._2)

  /**
   * Converts a DoubleGrid2D to a 2-dim Double-Array
   *
   * @param grid Grid to convert
   * @return Conversion result
   */
  def doubleGrid2Array(grid: DoubleGrid2D): Array[Array[Double]] = {
    val result: Array[Array[Double]] = Array.ofDim(grid.getHeight, grid.getWidth)

    for(i <- 0 until grid.getHeight; j <- 0 until grid.getWidth) {
      result(i)(j) = grid.get(i, j)
    }

    result
  }

  /**
   * Converts a two bags full of Int into a List[(Int, Int)]
   *
   * @param xBag x-coordinates
   * @param yBag y-coordinates
   * @return List of (Int, Int)
   */
  def toTupleList(xBag: IntBag, yBag: IntBag): List[(Int, Int)] = {
    assert(xBag.size() == yBag.size())

    var result: List[(Int, Int)] = List[(Int, Int)]()
    assert(result.isEmpty) // to check if above call acts like expected

    // Convert into List of (Int, Int)
    for (i <- 0 until xBag.size()) {
      val entry = (xBag.get(i), yBag.get(i))
      result = result.+:(entry)
    }

    assert(result.size == xBag.size())
    result
  }


  /**
   * Converts a two bags full of Int into a List[Int2D]
   *
   * @param xBag x-coordinates
   * @param yBag y-coordinates
   * @return List of Int2D
   */
  def toInt2DList(xBag: IntBag, yBag: IntBag): List[Int2D] = toTupleList(xBag, yBag).map(toInd2D)

  /**
   * Converts a bag full of ants to an ant list
   *
   * @param bag Bag to transform
   * @return List of ants in the given bag
   */
  def antBag2AntList(bag: Bag): List[Ant] = {
    val result = for (i: Int <- 0 until bag.size()) yield {
                   bag.get(i) match {
                     case ant: Ant => ant
                     case _        => throw new IllegalArgumentException("Bag does not consist only of ants.")
                   }
                 }

    result.toList
  }

  /**
   * Converts an existing, non empty
   *
   * @param a 2-dim array to convert
   * @return IntGrid with same content than the given 2-dim array
   */
  implicit def intArray2IntGrid(a: Array[Array[Int]]): IntGrid2D = {
    // ASSERT: Array not empty and 2-dim-Array has uniform width

    val result = new IntGrid2D(a.length, a(0).length, 0)

    for (i <- 0 until a.length; j <- 0 until a(0).length) {
      result.set(i, j, a(i)(j))
    }

    result
  }

  /**
   * Field Maximum Distance of two positions
   *
   * @param pos_a First position
   * @param pos_b Second position
   * @return Distance between first and second position
   */
  def maxDistance(pos_a: (Int, Int), pos_b: (Int, Int)): Int =
    max(abs(pos_a._1 - pos_b._1), abs(pos_a._2 - pos_b._2))


  /**
   * Field Mannheim Distance of two positions
   *
   * Manhattan Distance, Taxi Distance are other names for that.
   *
   * @param pos_a First position
   * @param pos_b Second position
   * @return Distance between first and second position
   */
  def mannheimDistance(pos_a: (Int, Int), pos_b: (Int, Int)): Int =
    max(abs(pos_a._1 - pos_b._1), abs(pos_a._2 - pos_b._2))


  /**
   * List of all positions in a given 2-dim-Array within a certain radius towards a position
   *
   * @param height Height of the 2-dim array
   * @param width Width of the 2-dim array
   * @param pos Position whichs neighbourhood will be determined
   * @param radius Radius of the neighbourhood
   * @return All positions within the `maxDistance`-radius of `pos`
   */
  def neighbourhoodOf(height: Int, width: Int, pos: (Int, Int), radius: Int): List[(Int, Int)] = {
    val allPositions: List[(Int, Int)] = (for (i <- 0 until height; j <- 0 until width) yield (i, j)).toList
    def predicate(otherPos: (Int, Int)): Boolean = maxDistance(pos, otherPos) <= radius

    allPositions.filter(predicate).toList
  }
}
