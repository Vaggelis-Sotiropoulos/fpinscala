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

  def maxPathLength[A](node: Tree[A]): Int = node match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + maxPathLength(l).max(maxPathLength(r))
  }

  def map[A, B](node: Tree[A])(f: A => B): Tree[B] = node match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](node: Tree[A], z: B)(f: (A, B) => B): B = node match {
    case Leaf(v) => f(v, z)
    case Branch(l, r) => fold(l, fold(r, z)(f))(f)
  }
}
