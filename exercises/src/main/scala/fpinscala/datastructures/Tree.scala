package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](node: Tree[A]): Int = {
    node match {
      case Leaf(_) => 1
      case Branch(l,r) => 1 + size(l) + size(r)
    }
  }

  def maximum(node: Tree[Int]): Int = {
    node match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l).max(maximum(r))
    }
  }

}