package hr.quora
import java.io.{InputStreamReader, FileReader, BufferedReader}

import scala.collection.mutable.HashMap
import scala.collection.mutable.{ListBuffer, ArrayBuffer}

/**
 *At Quora, we have aggregate graphs that track the number of upvotes we get each day.
As we looked at patterns across windows of certain sizes, we thought about ways to track trends such as
non-decreasing and non-increasing subranges as efficiently as possible.

For this problem, you are given N days of upvote count data, and a fixed window size K.
For each window of K days, from left to right, find the number of non-decreasing subranges within the window minus
the number of non-increasing subranges within the window.

A window of days is defined as contiguous range of days. Thus, there are exactly NK+1 windows where this metric
needs to be computed. A non-decreasing subrange is defined as a contiguous range of indices [a,b], a<b, where
each element is at least as large as the previous element. A non-increasing subrange is similarly defined, except
each element is at least as large as the next. There are up to K(K1)/2 of these respective subranges
within a window, so the metric is bounded by [K(K1)/2,K(K1)/2].
 */
object Upvotes {

  var buffer = ArrayBuffer.empty[Long]

  var a = ArrayBuffer.empty[Long]
  var b = ArrayBuffer.empty[Long]
  var c = ArrayBuffer.empty[Long]
  var d = ArrayBuffer.empty[Long]

  def main(args: Array[String]) {

    val in = new BufferedReader(new InputStreamReader(System in))
    val str = Stream.continually(in readLine)

    val nk = str.head
    val window = nk.split(" ")(1).toInt

    buffer = new ArrayBuffer[Long](nk.split(" ")(0).toInt)
    a = new ArrayBuffer[Long](nk.split(" ")(0).toInt)
    b = new ArrayBuffer[Long](nk.split(" ")(0).toInt)
    c = new ArrayBuffer[Long](nk.split(" ")(0).toInt)
    d = new ArrayBuffer[Long](nk.split(" ")(0).toInt)
    str.tail.head.split(" ").foreach( c => buffer.append(c.toLong))

    preprocess()
    getRanges(buffer, window)
  }

  private def getRanges(nums: ArrayBuffer[Long], window: Int): Unit = {

    var totalIncr = 0L
    var totalDecr = 0L
    for(i <- 0 until nums.size - window + 1) {
      if (i==0) {
        val diff = getResult(i, window + i)
        totalIncr = diff._1
        totalDecr = diff._2
        println(totalIncr - totalDecr)
      } else {
        var incr = 0L
        var decr = 0L
        // deal with increasing
        if (buffer(i-1) <= buffer(i)) {
          var endOfSet = b(i)
          if (endOfSet > window +i-1) {
            endOfSet = window +i - 1
          }
          val newSize = endOfSet - i + 1
          if (endOfSet == window +i -1 && buffer(i) >= buffer(i-1)) {
            incr -= 0
          } else {
            incr -= newSize
          }
        }

        if(buffer(window+i-1) >= buffer(window+i-2)) {
          var endOfSet = a(window+i-1)
          if (endOfSet < i) {
            endOfSet = i
          }
          val newSize =  window+i-1 - endOfSet
          if (endOfSet == i && buffer(i) >= buffer(i-1)) {
            incr += 0
          } else {
            incr += newSize
          }
        }

        // deal with decreasing
        if (buffer(i-1) >= buffer(i)) {
          var endOfSet = d(i)
          if (endOfSet > window +i -1) {
            endOfSet = window +i -1
          }
          val newSize = endOfSet - i + 1
          if (endOfSet == window +i  -1 && buffer(i) <= buffer(i-1)) {
            decr -= 0
          } else {
            decr -= newSize
          }
        }

        if(buffer(window+i-1) <= buffer(window+i-2)) {
          var endOfSet = c(window+i-1)
          if (endOfSet < i) {
            endOfSet = i
          }
          val newSize = window +i -1 - endOfSet
          if (endOfSet == i && buffer(i) <= buffer(i-1)) {
            decr += 0
          } else {
            decr += newSize
          }
        }

        totalIncr += incr
        totalDecr += decr

        println(totalIncr - totalDecr)
      }

    }

  }


  private def getResult(start: Int, end: Int): (Long, Long) = {
    var decr = 1L
    var incr = 1L

    var totalIncr = 0L
    var totalDecr = 0L
    var s = 0L
    var g = 0L
    var diff = 0L
    for (i <-start until end-1) {
      s = buffer(i)
      g = buffer(i+1)

      if (s <= g) {
        incr += 1
      } else {
        if(incr > 1) { totalIncr += (incr * (incr-1))/2 }
        incr = 1
      }

      if (incr > 1 && i == end-2) {
        totalIncr += (incr * (incr-1))/2
      }

      if ( s >= g) {
        decr += 1
      } else {
        if (decr > 1) { totalDecr += (decr * (decr-1))/2 }
        decr=1
      }

      if (decr > 1 && i == end-2) {
        totalDecr += (decr * (decr-1))/2
      }

    }

    (totalIncr, totalDecr)
  }

  def preprocess(): Unit = {
    //back incr
    var idx = 0
    a.append(idx)
    for (i <- 1 until buffer.size) {
      if (!(buffer(i) >= buffer(i-1))) {
        idx = i
      }
      a.append(idx)
    }

    //forward incr
    idx = buffer.size - 1
    b.append(idx)
    for (i <- buffer.size-1 to 1 by -1) {
      if (!(buffer(i) >= buffer(i-1))) {
        idx = i-1
      }
      b.append(idx)
    }
    b = b.reverse

    //back decr
    idx = 0
    c.append(idx)
    for (i <- 1 until buffer.size) {
      if (!(buffer(i) <= buffer(i-1))) {
        idx = i
      }
      c.append(idx)
    }

    //forward decr
    idx = buffer.size - 1
    d.append(idx)
    for (i <- buffer.size-1 to 1 by -1) {
      if (!(buffer(i) <= buffer(i-1))) {
        idx = i-1
      }
      d.append(idx)
    }
    d = d.reverse
  }

}
