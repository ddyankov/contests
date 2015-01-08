package hr.weeklyjan15

import scala.collection.mutable.HashMap

/**
 * Created by dimitar on 1/6/15.
 */
object Anagrams {

  def main(args: Array[String]) {
    val cases = readInt()

    for (i <- 0 until cases) {
      val lookup = new HashMap[String, Int]
      val str = readLine().toCharArray
      var total = 0
      for (m <- 0 until str.length) {
        for (n <- m until str.length) {
          val a = str.slice(m, n+1).sorted.toBuffer
          var s: String = ""
          a.foreach( t => s += t)
          //println(s)
          if (lookup.contains(s)) {
            lookup.put(s, lookup.get(s).get +1)
          } else if  (!s.isEmpty){
            lookup.put(s, 1)
          }
        }
      }
      lookup.foreach {
        case (k,v) =>
          if (v == 2) {
            total +=1
          } else if (v > 2) {
            total += ((v-1)*v)/2
          }
      }
      println(total)
    }
  }
}
