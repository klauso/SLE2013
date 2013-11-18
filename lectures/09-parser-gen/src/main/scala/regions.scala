package regions

/**
  * Let us define a small subset of a DSL to specify regions.
  *
  */

/**
  * Basic interface.
  */
trait Region

/**
  * A basic region, representing a unit circle centered at the origin of axes.
  */
case class UnitCircle() extends Region

/**
  * The union of two regions.
  */
case class UnionRegion(r1: Region, r2: Region) extends Region

/**
  * Scale a region by some factor.
  */
case class ScaleRegion(n: Double, r1: Region) extends Region

//Now, let's look at Regions.g
