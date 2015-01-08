package hr.weeklyjan15

import scala.collection.mutable.ListBuffer

/**
 * Created by dimitar on 1/5/15.
 */
object Bday {

  def main(args: Array[String]) {
    val cases = readLine().toInt

    for (i <-0 until cases) {
      val bw = readLine().split(" ")
      val xyz = readLine().split(" ")
      val b = bw(0).toLong
      val w = bw(1).toLong

      val x = xyz(0).toLong
      val y = xyz(1).toLong
      val z = xyz(2).toLong

      val a1 = x*b + y*w
      val a2 = (b+w) * x + w * z
      val a3 = (b+w) * y + b * z
      val buff = new ListBuffer[Long]
      buff.append(a1); buff.append(a2); buff.append(a3);
      println(buff.min)

    }
  }

}
