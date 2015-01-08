package hr.quora

import scala.collection.mutable._

/**
 * The Quora engineering team went to an archery offsite recently.
At the training corner, there was a target on the ground with a pile of arrows on it. Anna noticed that some of the arrows form the symbol 'Q' by intersecting the rings of the target.
The target is composed of N concentric circles and there are M arrows lying on it, each represented as a line segment.
The i-th circle is centered at the origin (0,0) and has radius Ri. The i-th arrow is a line segment with endpoints (x1i,y1i) and (x2i,y2i).
Now, Anna wonders if it is possible to write a program to quickly count the number of 'Q's formed. A 'Q' is defined as a pair of a circle and an arrow such that the arrow intersects the circumference of the circle exactly once.
 */
object Archery {

  case class Point(x1: Int, y1: Int, x2: Int, y2: Int)
  def main(args: Array[String]) {

    val lookup = new Array[Int](1000005)

    readLine()
    val radii = readLine().split(" ").map(_.toInt)
    val points = ListBuffer.empty[Point]

    val numPoints = readLine().toInt

    for(i <- 0 until numPoints) {
      val coords = readLine().split(" ").map(_.toInt)
      val x1 = coords(0)
      val y1 = coords(1)
      val x2 = coords(2)
      val y2 = coords(3)
      val point = {
        if (math.sqrt( (x1.toLong)*(x1.toLong) + (y1.toLong)*(y1.toLong) ) < math.sqrt( (x2.toLong)*(x2.toLong) + (y2.toLong)*(y2.toLong) )) {
          new Point(x1, y1, x2, y2)
        } else {
          new Point(x2, y2, x1, y1)
        }
      }
      points.append(point)
    }

    val rSet = new HashSet[Int]
    var max = 0
    for(r <- radii) {
      if ( r > max) {
        max = r
      }
      rSet.add(r)
    }

    for (i <- 1 to 1000004) {
      lookup(0) = 0
    }
    var curCount = 0
    for (i <- 1 to 1000004) {
      lookup(i) = curCount
      if (rSet.contains(i) && i != max) {
        curCount +=1
      }
      if ( i== max) {
        curCount +=1
      }
    }

    var count = 0
    for (p <- points) {
      var ceil = math.ceil(math.sqrt( (p.x1.toLong)*(p.x1.toLong) + (p.y1.toLong)*(p.y1.toLong) )).toLong
      var distCeil =  math.ceil(math.sqrt( (p.x2.toLong)*(p.x2.toLong) + (p.y2.toLong)*(p.y2.toLong) )).toLong
      if (distCeil > max) {
        distCeil = max + 1
      }
      if ( ceil > max) {
        ceil = max + 1
      }
      count += lookup(distCeil.toInt) - lookup(ceil.toInt)
    }

    println(count)
  }
}
