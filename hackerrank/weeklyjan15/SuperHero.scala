package hr.weeklyjan15

import java.util.Scanner
import scala.math._

object SuperHero {
  def main(args: Array[String]) {
    val sc = new Scanner(System.in)

    (0 until sc.nextByte()).foreach { _ =>
      val n = sc.nextInt()
      val m = sc.nextInt()

      val power = Array.tabulate[Int](n, m)((_, _) => sc.nextInt())
      val bullets = Array.tabulate[Int](n, m)((_, _) => sc.nextInt())

      val best = Array.tabulate[Array[(Int, Int)]](n)(i =>
        if (i < n - 1) findBest(sortByPower(power(i).zip(bullets(i))))
        else Array[(Int, Int)]((power(n - 1).min, 0))
      )

      println(solve(n-2, best(n-1), best).map(x => x._1 + x._2).min)
    }
  }

  def solve(n: Int, p: Array[(Int, Int)], best: Array[Array[(Int, Int)]] ): Array[(Int,Int)] = {
    if (n < 0) {
      p
    } else {
      val nextBest = best(n).map(a => (a._1, p.map(b => max(b._1 - a._2, 0) + b._2).min))
      solve(n-1, nextBest, best)
    }
  }

  def sortByPower(a: Array[(Int,Int)]) = a.sortWith( (x,y) => x._1 < y._1  || x._1 == y._1 && x._2 > y._2)

  def findBest(a: Array[(Int,Int)]) = {
    var max = 0
    a.filter { e =>
      if (e._2 <= max) false
      else {
        max = e._2
        true
      }
    }
  }
}