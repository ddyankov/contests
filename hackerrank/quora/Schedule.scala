package hr.quora

import scala.collection.mutable._
import scala.io.Source

/**
 * At Quora, we run all our unit tests across many machines in a test cluster on every code push.
One day, we decided to see if we could optimize our test cluster for cost efficiency by using only one machine to run all N tests.
Suppose we know two things about each test: the time needed to run this test, Ti, and the probability that this test will pass, Pi.
Given these as input, come up with the minimum expected time (based on the optimal ordering of the tests) of getting go or no go
feedback on the code push, i.e.the expected time when we understand that either i) at least one test has failed, or that ii) all
tests have passed.
 */

case class Test(time: Int, prob: Double)

object Schedule {

  def main(args: Array[String]) {

    val lines = Source.stdin.getLines()
    val cases = lines.next().toInt

    val tests = ListBuffer.empty[Test]

    for (i <- 0 until cases) {
      val vals = lines.next().split(" ")
      tests.append(new Test(vals(0).toInt, vals(1).toDouble))
    }

    val permutations = tests.permutations.buffered
    var min = Double.MaxValue
    if (cases <= 12) {
      while (permutations.hasNext) {
        val perm = permutations.next()
        val score = calcProbability(perm)
        if (score < min) {
          min = score
        }
      }
      println(min)
    } else {
      val values = ListBuffer.empty[Double]
      val start = calcProbability(tests)
      values.append(start)

      val sortedTimeDescSeq = sortTimeDesc(tests)
      val sortedTimeAscSeq = sortTimeAsc(tests)
      val sortedProbAsc = sortProbAsc(tests)
      val sortedProbDesc = sortProbDesc(tests)
      val sortedExpectedFail = sortByExpectedFail(tests)
      val sortedExpectedSuccess = sortByExpectedSuccess(tests)
      val sortedExpectedSuccess2 = sortByExpectedSuccess2(tests)
      val sortedExpectedFail2 = sortByExpectedFail2(tests)


      val p1 = findTheBest(sortedTimeDescSeq, values.min, 0)
      values.append(p1)

      val p2 = findTheBest(sortedTimeAscSeq, values.min, 0)
      values.append(p2)

      val p3 = findTheBest(sortedProbDesc, values.min, 0)
      values.append(p3)

      val p4 = findTheBest(sortedProbAsc, values.min, 0)
      values.append(p4)

      val p5 = findTheBest(sortedExpectedFail, values.min, 0)
      values.append(p5)

      val p6 = findTheBest(sortedExpectedSuccess, values.min, 0)
      values.append(p6)

      val p7 = findTheBest(sortedExpectedFail2, values.min, 0)
      values.append(p7)

      val p8 = findTheBest(sortedExpectedSuccess2, values.min, 0)
      values.append(p8)

      println(values.min)
    }
  }

  private def sortTimeDesc(tests: ListBuffer[Test]): ListBuffer[Test] = {
    tests.sortBy(r =>(r.time, r.prob))
  }

  private def sortProbDesc(tests: ListBuffer[Test]): ListBuffer[Test] = {
    tests.sortBy(r =>(r.prob, r.time))
  }

  private def sortTimeAsc(tests: ListBuffer[Test]): ListBuffer[Test] = {
    tests.sortBy(r =>(-r.time, r.prob))
  }

  private def sortProbAsc(tests: ListBuffer[Test]): ListBuffer[Test] = {
    tests.sortBy(r =>(-r.prob, r.time))
  }

  private def sortByExpectedFail(tests: ListBuffer[Test]): ListBuffer[Test] = {
    tests.sortBy(r => (r.time * (1 - r.prob)))
  }

  private def sortByExpectedFail2(tests: ListBuffer[Test]): ListBuffer[Test] = {
    tests.sortBy(r => -(r.time * (1 - r.prob)))
  }

  private def sortByExpectedSuccess(tests: ListBuffer[Test]): ListBuffer[Test] = {
    tests.sortBy(r => (r.time * r.prob))
  }

  private def sortByExpectedSuccess2(tests: ListBuffer[Test]): ListBuffer[Test] = {
    tests.sortBy(r => -(r.time * r.prob))
  }

  private def findTheBest(list: ListBuffer[Test], min: Double, idx: Int) : Double = {
    if ( idx >= list.size) {
      return min
    }
    var res = ListBuffer.empty[Test]
    var curMin = Double.MaxValue
    for (y <- 0 to list.size-1) {
      val newList = placeAtPosition(list.slice(0, idx) ++ list.slice(idx+1, list.size), list(idx), y)
      val score = calcProbability(newList)
      if (score < curMin) {
        curMin = score
        res = newList
      }
    }

    if (curMin < min) {
      findTheBest(res, curMin, idx+1)
    } else {
      findTheBest(res, min, idx + 1)
    }
  }

  private def placeAtPosition(tests: ListBuffer[Test], element: Test, pos: Int): ListBuffer[Test] = {
    val newList = ListBuffer.empty[Test]
    var pointer = 0
    for (i <- 0 to tests.size) {
      if (i == pos) {
        newList.append(element)
      } else {
        newList.append(tests(pointer))
        pointer += 1
      }
    }
    newList
  }

  private def calcProbability(tests: ListBuffer[Test]): Double = {
    var result: Double = 0
    var prev: Double = 1
    for (i <- 0 until tests.size) {
      result += ( tests(i).time * tests(i).prob + tests(i).time * (1 - tests(i).prob) ) * prev
      prev = prev * tests(i).prob
    }
    return result
  }

}
