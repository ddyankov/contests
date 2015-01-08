package hr.weeklyjan15
import scala.collection.mutable.{ListBuffer, HashMap}

/**
 * Created by dimitar on 1/7/15.
 */

case class Enemy(var power: Int, var bullets: Int, var path: Int)

object SuperHero {

  def main (args: Array[String] ) {
    val cases = readInt()

    for (i<-0 until cases) {
      val lookup = new HashMap[Int, ListBuffer[Enemy]]
      val nm = readLine().split(" ")
      val levels = nm(0).toInt
      val enemies = nm(1).toInt

      for (l <-1 to levels) {
        val e = readLine().split(" ")
        val buff = new ListBuffer[Enemy]
        lookup.put(l, buff)
        e.foreach(v => lookup.get(l).get.append(new Enemy(v.toInt, 0, 0)))
      }

      for (l <-1 to levels) {
        val b = readLine().split(" ")
        var count = 0
        b.foreach{
          v => lookup.get(l).get(count).bullets=v.toInt
            count +=1
        }
      }

      lookup.foreach {
        case (k,v) =>
          val sortedV = v.sortBy(a=> (a.power, -a.bullets))
          lookup.put(k, sortedV)
      }

      val pick = lookup.get(levels).get(0).power
      // set initial path
      lookup.get(levels).get.foreach{
        en => en.path = pick
      }

      for (k <- levels-1 to 1 by -1) {
        val c = lookup.get(k).get
        for (t <-0 until c.size) {
          val enemy = c(t)

        }

      }


    }
  }
}
