package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](node: Tree[A]): Int =
    fold(node)(a => 1)(1 + _ + _)

  def maximum(node: Tree[Int]): Int =
    fold(node)(a => a)(_ max _)

  def maxPathLength[A](node: Tree[A]): Int =
    fold(node)(a => 0)((d1,d2) => 1 + (d1 max d2))

  def map[A, B](node: Tree[A])(f: A => B): Tree[B] =
    fold(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  def fold[A,B](node: Tree[A])(f: A => B)(g: (B,B) => B): B = node match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
}
