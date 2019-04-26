package chapter_three_tests

import org.scalatest.FunSuite
import chapter_three.datastructures._
import chapter_three.Exercises._

class ExercisesTest extends FunSuite {
  val l = List(1,2,3,4,5,6)

  test("drop") {
    assert(drop(l, 2) == List(3,4,5,6))
  }

  test("dropWhile") {
    assert(dropWhile(l, (x: Int) => x < 4) == List(4,5,6))
  }

  test("init") {
    assert(init(l) == List(1,2,3,4,5))
  }

  test("length") {
    assert(length(l) == 6)
  }

  test("reverse") {
    assert(reverse(l) == List(6,5,4,3,2,1))
  }

  test("fold left implemented with fold right") {
    assert(fl_in_fr(l)(1:Int)((acc: Int, x: Int) => acc / x)
      ==
    foldLeft(l)(1)(_ / _))
  }

  test("fold right implemented with fold left") {
    assert(fr_in_fl(l)(1:Int)((x: Int, acc: Int) => acc / x)
      ==
    List.foldRight(l)(1)((x: Int, acc: Int) => acc / x))
  }

  test("append") {
    assert(append(l, l) == List(1,2,3,4,5,6,1,2,3,4,5,6))
  }

  test("concat") {
    assert(concat(List(List(1,2,3), List(4,5,6), List(1,2,3))) == List(1,2,3,4,5,6,1,2,3))
  }

  test("incall") {
    assert(incall(l) == List(2,3,4,5,6,7))
  }

  test("flatMap") {
    assert(flatMap(l)((a: Int) => List(a, a * a)) == List(1,1,2,4,3,9,4,16,5,25,6,36))
  }

  test("zipWith") {
    assert(zipWith(l, l)(_ + _) == List(2,4,6,8,10,12))
  }

  test("startsWith") {
    assert(startsWith(List(1,2,3,4), Nil))
    assert(startsWith(List(1,2,3,4), List(1)))
    assert(!startsWith(List(2,3,4), List(1)))
    assert(!startsWith(Nil, List(1)))
  }

  test("hasSubsequence") {
    assert(hasSubsequence(List(1,2,3,4,5), List(2,3)))
    assert(hasSubsequence(List(1,2,3,4,5), Nil))
    assert(hasSubsequence(List(1,2,3,4,5), List(1,2)))
    assert(!hasSubsequence(List(1,2,3,4,5), List(1,3)))
    assert(!hasSubsequence(Nil, List(1,3)))
  }

  val t = Branch(Branch(Leaf("a"), Leaf("b")),
    Branch(Leaf("c"), Leaf("d")))

  // 3.25
  test("tree size") {
    assert(size(t) == 7)
  }

  // 3.26
  test("tree maximum") {
    val tree = Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))
    assert(maximum(tree) == 4)
  }

  // 3.27
  test("tree depth") {
    assert(depth(t) == 2)
  }
}
