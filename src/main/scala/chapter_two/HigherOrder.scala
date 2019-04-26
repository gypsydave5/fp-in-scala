package chapter_two

import scala.annotation.tailrec

object HigherOrder {
  def factorial(n:Int): Int = {
    //named inner function scoped to outer
    @tailrec
    def helper(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else helper(n - 1, n * acc)

    helper(n, 1)
  }
}
