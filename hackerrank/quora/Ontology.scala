package hr.quora

import scala.collection.mutable
import scala.collection.mutable._

/**
 * Created by dimitar on 12/14/14.
 */
object Ontology {

  class Node(var name: String, var parent: Node, var children: ArrayBuffer[Node], var questions: ArrayBuffer[String])

  var root: Node = null
  val questions = new HashMap[String, ArrayBuffer[String]]
  val lookup = new HashMap[String, Node]

  def main(args: Array[String]) {

    val topicsCount = readLine()
    val nodes = readLine()

    val qs = readLine().toInt
    for (i <- 0 until qs) {
      val q = readLine().split(":")

      if (questions.get(q(0).trim).isDefined) {
        questions.get(q(0).trim).get.append(q(1).trim)
      } else {
        val nq = new ArrayBuffer[String]
        nq.append(q(1).trim)
        questions.put(q(0).trim, nq)
      }
    }
    buildTree(nodes.trim)

    val testsNum = readLine().toInt
    for (i <- 0 until testsNum) {
      val test = readLine()
      val space = test.indexOf(" ")
      val who = test.substring(0, space+1).trim
      val what = test.substring(space+1, test.size).trim

      val count = search(who, what)
      println(count)
    }
  }

  def search(who: String, what:String): Int = {
    var count = 0
    val stack = new Stack[Node]
    stack.push(lookup.get(who).get)
    while (!stack.isEmpty) {
      val node = stack.pop

      node.questions foreach {
        q => if (q.trim.startsWith(what.trim)) {
          count += 1
        }
      }
      node.children foreach(stack.push(_))
    }
    count
  }


  def buildTree(nodes: String): Unit = {

    var term = ""
    val stack = new Stack[Node]

    for (i<-0 until nodes.size) {
      if (nodes.charAt(i).toString.equals("(")) {
        if (stack.isEmpty) {
          val q = questions.get(term.trim).getOrElse(new ArrayBuffer[String])
          val node = new Node(term.trim, null, new ArrayBuffer[Node], q)
          root = node
          lookup.put(node.name, node)
          stack.push(node)
        } else {
          if (!term.isEmpty) {
            val ns = term.trim.split(" ")
            val p = stack.top

            ns foreach { s => val q = questions.get(s.trim).getOrElse(new ArrayBuffer[String]);
              if (!s.isEmpty) {
                val child = new Node(s.trim, p, new ArrayBuffer[Node], q)
                p.children.append(child)
                lookup.put(child.name, child)
              }
            }

            val newRoot = p.children(p.children.size - 1)
            stack.push(newRoot)
          }
        }
        term = ""
      }

      if (nodes.charAt(i).toString.equals(")")) {
        if (!term.trim.isEmpty) {

          val str = term.trim.split(" ")
          val top = stack.pop()

          str foreach { s => val q = questions.get(s.trim).getOrElse(new ArrayBuffer[String]);
            if (!s.isEmpty) {
              val child = new Node(s.trim, top, new ArrayBuffer[Node], q)
              top.children.append(child)
              lookup.put(child.name, child)
            }
          }
        } else {
          stack.pop()
        }
        term = ""
      }

      if (!nodes.charAt(i).toString.equals(")") && !nodes.charAt(i).toString.equals("(")) {
        term += nodes.charAt(i).toString
      }
    }

  }
}
