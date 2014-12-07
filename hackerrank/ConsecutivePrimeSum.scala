import scala.collection.mutable._

/**
 *The prime 41, can be written as the sum of six consecutive primes: 41=2+3+5+7+11+13
 * This is the longest sum of consecutive primes that adds to a prime below one-hundred.
 * The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.
 * Which prime, N, can be written as the sum of the most consecutive primes?
 *  2 <= N <= 10^12
 */

object ConsecutivePrimeSum {

  val sums = HashMap.empty[Int, Long]
  var primes = ArrayBuffer.empty[Int]

  def main(args: Array[String]) {

    val cases = readLine().toInt

    val all = ListBuffer.empty[Long]
    for (i <- 0 until cases) {
      all.append(readLine().toLong)
    }

    val max = all.max
    init(max)

    val maxPrimes = math.sqrt(max).toInt
    primes = new ArrayBuffer[Int](maxPrimes)
    initPrimes(maxPrimes)

    val sorted = sums.values.toList.sorted

    for (i <- 0 until all.size) {

      val num = all(i)
      val result: ListBuffer[(Long, Int)] = ListBuffer.empty[(Long, Int)]
      val end = endIdx(num, sorted)

      // if input is 2 - directly return the result
      var len = 0
      var maxLen = 0
      var loop = true
      if (num == 2 ) {
        result.append((2,1))
      } else {
        for (startIdx <- 0 to end if loop) {
          if (end - startIdx < maxLen) {
            loop = false
          }
          val (x, y): (Long, Int) = getBest(0, 0, num, len, end, startIdx)
          len = y
          if (len > maxLen) {
            maxLen = len
            result.append((x,y))
          }
        }
      }
      val r = result.maxBy(_._2)
      println(r._1 + " " + r._2)
    }

  }

  private def initPrimes(limit: Int): Unit = {
    primes.append(2)
      for(i <- 3 to limit by + 2) {
        if (isPrime(i)) {
          primes.append(i)
        }
      }
  }

  private def init(max: Long): Unit = {
    var sum = 2L
    sums.put(0, 2)

    var i = 3
    var idx = 1
    var loop = true

    while (loop ) {
      if(isPrime(i)) {
        sum += i
        sums.put(idx, sum)
        idx += 1
      }
      if (sum > max) {
        loop = false
      }
      i += 2

    }
  }

  def endIdx(num: Long, listNums: List[Long]): Int = {
    listNums.indexOf(listNums.minBy(v => math.abs(v - num)))
  }

  private def getBest(len: Int, s: Long, n: Long, targetLen: Int, ri: Int, start: Int): (Long, Int) = {
    if (ri - start + 1 < targetLen) {
      return (s, len)
    }

    if (ri <= 0) {
      return (s,len)
    }
    val sum = sums.get(ri).get - sums.get(start).get + primes(start)
    if (sum > n ) {
      return getBest(len, s, n, targetLen, ri-1, start)
    }

    val isSumPrime: Boolean = isPrime(sum)
    if (isSumPrime && len <= (ri - start + 1)) {
      return getBest(ri-start+1, sum, n, targetLen, ri-1, start)
    } else if (!isSumPrime && (ri - start +1) >= len) {
      return getBest(len, s, n, targetLen, ri-1, start)
    } else {
      return (s, len)
    }
  }


  private def isPrime(n: Long): Boolean = {

    if (n == 2 || n == 3 || n == 5 || n == 7)
      return true;
    if (n % 2 == 0 || n % 3 == 0 || n % 5 == 0 || n % 7 == 0)
      return false;

    val sqrt = math.sqrt(n).toInt + 1
    check(n, sqrt, 6)
  }

  /**
   * Check for 2 & 3 includes all even numbers + all numbers divisible by 3(=> divisible by 6 as well).
   * Therefore we subtract 1 and add 1 to div to cover the other cases.
   */
  private def check(n:Long, sqrt: Int, div: Int): Boolean = {
    if (div - sqrt > 0) {
      return true
    }

    if ((n % (div-1)) == 0 ||  (n % (div+1)) == 0) {
      return false
    } else {
      return check(n, sqrt, div+6)
    }
  }
}
