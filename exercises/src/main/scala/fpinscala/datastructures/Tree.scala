package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](node: Tree[A]): Int = node match {
      case Leaf(_) => 1
      case Branch(l,r) => 1 + size(l) + size(r)
    }

  def maximum(node: Tree[Int]): Int = node match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l).max(maximum(r))
    }

  def maxPathLength[A](node: Tree[A]): Int = {
    def pathCount(node1: Tree[A], numPath: Int): Int ={
      val numPath2 = numPath + 1
      node1 match {
        case Leaf(_) => numPath2
        case Branch(l, r) => pathCount(l, numPath2).max(pathCount(r, numPath2))
      }
    }
    pathCount(node, 0)
  }

}
