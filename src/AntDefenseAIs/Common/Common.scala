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
package AntDefenseAIs.Common

import sim.util.{Bag, IntBag, Int2D}
import AntDefenseAIs.Model.Ant
import sim.field.grid.IntGrid2D

/** Helping functions and constants */
package object Common {

  /**
   * Converts Int2D to (Int, Int)
   */
  def toTuple(i: Int2D): (Int, Int) = (i.getX, i.getY)

  /** Converts (Int, Int) to Int2D */
  def toInd2D(t: (Int, Int)): Int2D = new Int2D(t._1, t._2)

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
  def toInt2DList(xBag: IntBag, yBag: IntBag): List[Int2D] = toTupleList(xBag, yBag) map toInd2D

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
  def intArray2IntGrid(a: Array[Array[Int]]): IntGrid2D = {
    // ASSERT: Array not empty and 2-dim-Array has uniform width

    val result = new IntGrid2D(a.length, a(0).length, 0)

    for (i <- 0 until a.length; j <- 0 until a(0).length) {
      result.set(i, j, a(i)(j))
    }

    result
  }
}
