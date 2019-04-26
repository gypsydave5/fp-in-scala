package chapter_two

object Exercises {
  // 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def helper(a: Int, b: Int, n: Int): Int =
      if (n == 0) a
      else helper(b, a + b, n - 1)

    helper(0, 1, n)
  }

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def helper(a: Int, b: Int): Boolean =
      if (b == as.length) true
      else if (!ordered(as(a), as(b))) false
      else helper(a + 1, b + 1)

    helper(0, 1)
  }

  // 2.3
  def curry[A,B,C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)

  // 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
