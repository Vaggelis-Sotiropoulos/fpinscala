package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](node: Tree[A]): Int = {
    def count(node1: Tree[A], count1: Int): Int = {
      node1 match {
        case Branch(None, None) => count1 + 1
        case Branch(left, right) => count(left, count1) + count(right, count1)
      }
    }
    count(node, 1)
  }


}