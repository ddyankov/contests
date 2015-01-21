package hackercup2015.round1

import java.util.Scanner

import scala.collection.mutable.ListBuffer

/**
 * Created by dimitar on 1/17/15.
 */
object Sports {

  val mod = 1000000007
  val dp = Array.ofDim[Long](2002,2002)
  val dp1 = Array.ofDim[Long](2002,2002)

  def main(args: Array[String]) {
    val sc = new Scanner(System.in)

    val t = sc.nextInt()

    sc.nextLine()
    (0 until t).foreach { c =>

      val line = sc.nextLine().split("-")
      val s1 = line(0).toInt
      val s2 = line(1).toInt

      stressfree(s1,s2)
      stressfull(s1,s2)
      println("Case #" +(c+1) +": " + dp(s1)(s2) + " " + dp1(s1)(s2))
    }
  }

  def stressfree(c1: Int, c2: Int): Long = {

    if (c1 == 1 && c2 == 0) {
      dp(c1)(c2) = 1
      return 1
    }

    if (dp(c1)(c2) > 0 ) {
      return dp(c1)(c2)
    }

    var ret = 0L
    if (c1 > 0 && c1 - 1 > c2) {
      ret += stressfree(c1 - 1, c2) % mod
    }
    if (c2 > 0) {
      ret += stressfree(c1, c2 - 1) % mod
    }

    dp(c1)(c2) = ret % mod
    ret % mod
  }

  def stressfull(c1: Int, c2: Int): Long = {
    if (c1 == 0 && c2 == 0) {
      dp1(c1)(c1) = 1
      return 1
    }

    if (dp1(c1)(c2) > 0 ) {
      return dp1(c1)(c2)
    }

    var ret = 0L
    if (c1 > 0) {
      ret += stressfull(c1-1, c2) % mod
    }
    if ( (c2-1) >= c1 ) {
      ret += stressfull(c1, c2 - 1) % mod
    }

    dp1(c1)(c2) = ret % mod
    ret % mod
  }
}


