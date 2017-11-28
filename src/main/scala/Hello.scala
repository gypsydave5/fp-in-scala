import trees.{Branch, Leaf, Tree}
import trees.Tree._

object Hello extends App {
  println("Hello, World!")
}

object main extends App {
  def sizeF2[_](t: Tree[_]): Int = foldL(t)(0)((acc, _) => acc + 1)
  def sizeF[_](t: Tree[_]): Int = foldLR(t)(0)((acc, _) => acc + 1)

  def sumF(t: Tree[Int]): Int = foldL(t)(0)((acc, n) => acc + n)

  def divF(t: Tree[Int]): Int = foldL(t)(10000000)((acc, n) => acc / n)

  val exampleTree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))
  val smallTree = Branch(Leaf(100), Leaf(10))

  println(size(exampleTree))
  println(sizeF(exampleTree))
  println(sizeF2(exampleTree))

  println(divF(smallTree))
  println(8/4)
}