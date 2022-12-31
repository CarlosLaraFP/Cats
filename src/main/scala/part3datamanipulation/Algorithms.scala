package part3datamanipulation

import cats.Eval

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Algorithms {

  class TreeNode(_value: Int = 0, _left: Option[TreeNode] = None, _right: Option[TreeNode] = None) {
    // Mutable data structure
    var value: Int = _value
    var left: Option[TreeNode] = _left
    var right: Option[TreeNode] = _right
  }

  //@tailrec
  def createTree(depth: Int): TreeNode = {
    if (depth == 0) new TreeNode
    else {
      val left = createTree(depth - 1)
      val right = createTree(depth - 1)
      new TreeNode(depth, Some(left), Some(right))
    }
  }

  def createBalancedTree(depth: Int): Eval[TreeNode] = {
    /*
      Amazing. Not only can head recursive methods be converted into tail recursive methods, but
      Eval[T] contains flatMap and map methods, enabling [deferred] for-comprehensions.
      Wow, tail recursion for life using Cats, guaranteed at compile time.
    */
    if (depth == 0) Eval.now(new TreeNode)
    else Eval defer {
      for {
        left <- createBalancedTree(depth - 1)
        right <- createBalancedTree(depth - 1)
      } yield new TreeNode(depth, Some(left), Some(right))
    }
  }

  def depthFirstSearch(root: Option[TreeNode]): Vector[Int] = {
    /*
      Return the total number of elements in the tree using DFS pre-order traversal.
      O(N) time complexity
      O(N) space complexity due to head recursion
    */
    //@tailrec
    def dfsHelper(node: Option[TreeNode], elements: Vector[Int]): Eval[Vector[Int]] = node match {
      case None => Eval.now(elements)
      case Some(x) => Eval.defer {
        val left = dfsHelper(x.left, elements appended x.value)
        dfsHelper(x.right, left.value)
      }
    }
    dfsHelper(root, Vector.empty[Int]).value//.size
  }


  def main(args: Array[String]): Unit = {
    //
    val root = createTree(4)
    println(root.value)
    println(depthFirstSearch(Some(root)))
    val rootGpt = createBalancedTree(4)
    println(rootGpt.value.value)
    println(depthFirstSearch(Some(rootGpt.value)))
  }
}
