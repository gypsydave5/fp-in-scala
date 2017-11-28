import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// Exercise 3.25

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
def map[A,B](t: Tree[A])(f: (A) => B): Tree[B] = t match {
  case Leaf(a) => Leaf(f(a))
  case Branch(left, right) => Branch(map(left)(f), map(right)(f))
}

// Exercise 3.29
def fold[A, B](t: Tree[A], z: B, f: (B, A) => B): B = t match {
  case Leaf(a) => f(z, a)
  case Branch(left, right) => fold(right, fold(left, z, f), f)
}

def foldL[A, B](t: Tree[A])(z: B)(f: (B, A) => B): B = {
  @tailrec def recur(z: B)(t: Tree[A])(f: ((B, A) => B))(cont: (B) => B): B = t match {
    case Leaf(a) => cont(f(z, a))
    case Branch(left, right) => recur(z) (left) (f) ((b: B) => recur(b)(right)(f)(cont))
  }

  recur(z)(t)(f)((b: B) => b)
}

def sizeF[A](t: Tree[A]): Int = foldL(t)(0)((acc, _) => acc + 1)

val exampleTree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

size(exampleTree) == sizeF(exampleTree)