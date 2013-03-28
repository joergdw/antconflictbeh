package Setup

import sim.util.{IntBag, Int2D}

/** Helping functions and constants */
object Helpers {

  /** Converts Int2D to (Int, Int) */
  def toTuple(i: Int2D): (Int, Int) = (i.getX, i.getY)

  /** Converts (Int, Int) to Int2D */
  def toInd2D(t: (Int, Int)): Int2D = new Int2D(t._1, t._2)

  /** Converts two IntBags to List[(Int, Int)] */
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

  def toInt2DList(xBag: IntBag, yBag: IntBag): List[Int2D] = toTupleList(xBag, yBag) map toInd2D
}
