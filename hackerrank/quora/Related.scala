package hr.quora

import scala.collection.mutable._

/**
 * For the purposes of this problem, suppose Quora has N questions, and question i (1iN) takes Ti time to read.
 * There exists exactly one path from any question to another, and related questions form undirected pairs between themselves.
 * In other words, the graph of related questions is a tree.

Each time Steve reads a question, he will see a list of related questions and navigate to one that he hasn't read yet at random.
Steve will stop reading once there are no unread related questions.

Which question should we show first to Steve so that we minimize his total expected reading time?
It is guaranteed that there is one unique question that is optimal.
 */
object Related {

  val related = new HashMap[Int, ArrayBuffer[Int]]
  var allNodes = new ArrayBuffer[Int](100000)
  var times = new ArrayBuffer[Int]

  val next = new HashMap[String, ArrayBuffer[Int]]

  def main(args: Array[String]) {

    val questions = readLine().toInt
    readLine().split(" ").foreach(c => times.append(c.toInt))

    for (i<- 1 to questions -1 ) {
      val nodes = readLine().split(" ")
      val parent = nodes(0).toInt
      val child = nodes(1).toInt

      if (related.contains(parent)) {
        related.get(parent).get.append(child)
      } else {
        val newVal = new ArrayBuffer[Int]
        newVal.append(child)
        related.put(parent, newVal)
      }

      if (related.contains(child)) {
        related.get(child).get.append(parent)
      } else {
        val newVal = new ArrayBuffer[Int]
        newVal.append(parent)
        related.put(child, newVal)
      }

      allNodes.append(parent)
      allNodes.append(child)
    }


    allNodes = allNodes.distinct.sorted

    related foreach {
      case (k,v) =>
        if (v.size == 1) {
          next.put("none,"+k, v)
        } else {
          v foreach {
            e =>
              next.put(e.toString+","+k.toString, v.filter(c => c!=e))
          }
        }
    }

    println(next)
    //println(times)
    println(related)

    println(getResult())
  }

  private def getResult(): Int = {
    var result = 0
    var time = Double.MaxValue


    for(i <- 0 until allNodes.size) {
      val timeMap = new HashMap[String, Double]
      timeMap.put("tt", 0d)
      val node = allNodes(i)

      val children = related.get(node).getOrElse(ArrayBuffer.empty)

      var score = 0d
      if (!children.isEmpty) {
        children foreach { c =>
          calcExpectedTime(c, 1 / children.size.toDouble,
            times(c - 1)*1 / children.size.toDouble, timeMap, node)
        }
      }

      score = timeMap.get("tt").get + times(node-1)

      println(node, score)
      val total = score
      if (total < time) {
        time = total
        result = node
      }
    }

    result
  }

  private def calcExpectedTime(node: Int, prob: Double, t: Double,
                                 timeMap: HashMap[String, Double], prev: Int): Unit = {
    if (node == -1) {
      val newVal = timeMap.get("tt").get + t
      timeMap.put("tt", newVal)
    } else {
      val children = next.get(prev+","+node).getOrElse(ArrayBuffer.empty)
      if (children.isEmpty) {
        calcExpectedTime(-1, prob * 1 / 2d, t, timeMap, node)
      } else {
        val newVal = timeMap.get("tt").get + t
        timeMap.put("tt", newVal)

        children foreach { c =>
            calcExpectedTime(c, prob * 1 / children.size.toDouble,
              times(c - 1) * prob * 1 / children.size.toDouble, timeMap, node)
        }
      }
    }
  }

}
