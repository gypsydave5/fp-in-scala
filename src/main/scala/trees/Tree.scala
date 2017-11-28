package trees

import scala.annotation.tailrec

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// Exercise 3.25

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right)
  }

  // Exercise 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  // Exercise 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(left, right) => depth(left) max depth(right)
  }

  // Exercise 3.28
  def map[A, B](t: Tree[A])(f: (A) => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  // Exercise 3.29
  def fold[A, B](t: Tree[A], z: B, f: (B, A) => B): B = t match {
    case Leaf(a) => f(z, a)
    case Branch(left, right) => fold(right, fold(left, z, f), f)
  }

  // Scala doesn't like passing a continuation for tail calls
  def foldLR[A, B](t: Tree[A])(z: B)(f: (B, A) => B): B = {
    def recur(z: B) (t: Tree[A]) (f: ((B, A) => B)) (cont: (B) => B): B = t match {
      case Leaf(a) => cont(f(z, a))
      case Branch(left, right) => recur(z) (left) (f) ( (b: B) => recur (b) (right) (f) (cont) )
    }

    recur(z)(t)(f)((b: B) => b)
  }

  // much prefers a data structure
  def foldL[A, B](t: Tree[A])(z: B)(f: (B, A) => B): B = {
    @tailrec def recur(z: B) (todo: List[Tree[A]]) (f: ((B, A) => B)): B = todo match {
      case Nil => z
      case Leaf(a)::rest => recur(f(z, a)) (rest) (f)
      case Branch(left, right)::rest => recur(z) (left::right::rest) (f)
    }

    recur(z)(t::Nil)(f)
  }

  def foldR[A, B](t: Tree[A])(z: B)(f: (B, A) => B): B = {
    @tailrec def recur(z: B) (todo: List[Tree[A]]) (f: ((B, A) => B)): B = todo match {
      case Nil => z
      case Leaf(a)::rest => recur(f(z, a)) (rest) (f)
      case Branch(left, right)::rest => recur(z) (right::left::rest) (f)
    }

    recur(z)(t::Nil)(f)
  }
}
