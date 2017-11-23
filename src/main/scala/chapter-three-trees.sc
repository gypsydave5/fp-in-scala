sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

val tree = Branch(
  Branch(
    Leaf(5), Branch(Leaf(7), Leaf(8))
  ),
  Branch(
    Leaf(3),Leaf(4)
  )
)

def size[A](tree: Tree[A]): Int = tree match {
  case Leaf(_) => 1
  case Branch(t1, t2) => 1 + size(t1) + size(t2)
}

def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(l1, l2) => maximum(l1) max maximum(l2)
}

def depth(tree: Tree[Int]): Int = {
  def recur(tree: Tree[Int], depth: Int): Int = tree match {
    case Leaf(_) => depth
    case Branch(l1, l2) => recur(l1, depth + 1) max recur(l2, depth + 1)
  }

  recur(tree, 1)
}

def map[A, B](tree: Tree[A])(f: (A) => B): Tree[B] = tree match {

}

size(tree)
maximum(tree)
depth(tree)