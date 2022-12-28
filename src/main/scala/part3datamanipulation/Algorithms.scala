package part3datamanipulation

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Algorithms {

  class TreeNode(_value: Int = 0, _left: Option[TreeNode], _right: Option[TreeNode]) {
    // Mutable data structure
    var value: Int = _value
    var left: Option[TreeNode] = _left
    var right: Option[TreeNode] = _right
  }

  //@tailrec
  def createTree(depth: Int): Option[TreeNode] = {
    if (depth == 0) None
    else Some(new TreeNode(depth, createTree(depth - 1), createTree(depth - 1)))
  }

  def depthFirstSearch(root: Option[TreeNode]): Vector[Int] = {
    /*
      Return the total number of elements in the tree using DFS pre-order traversal.
      O(N) time complexity
      O(N) space complexity due to head recursion
    */
    //@tailrec
    def dfsHelper(node: Option[TreeNode], elements: Vector[Int]): Vector[Int] = node match {
      case None => elements
      case Some(x) =>
        val left = dfsHelper(x.left, elements appended x.value)
        dfsHelper(x.right, left)
    }
    dfsHelper(root, Vector.empty[Int])
  }

  def main(args: Array[String]): Unit = {
    //
    val root = createTree(400)
    //val elements = depthFirstSearch(root)
    //println(elements)
  }
}
