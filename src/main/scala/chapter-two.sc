import scala.annotation.tailrec

object MyModule {
  def abs(n: Int) =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
}

object AnotherModule extends ((Int) => Int) {
  override def apply(n: Int) = n + 1
}

AnotherModule(5)
AnotherModule.apply(5)

object Module3 {
  val number = MyModule.abs(-7)
}

//Factorial
def factorial(n: Int): Int = {
  @tailrec
  def go(n: Int, acc: Int): Int =
    if (n <= 0) acc
    else go(n - 1, n * acc)

  go(n, 1)
}

def factorialTwo(n: Int): Int =
  (1 until (n + 1)).product

def factorialThree(n: Int): Int =
  if (n == 1) 1
  else n * factorialThree(n - 1)

factorial(5)
factorialTwo(5)
factorialThree(5)

// Exercise 2.1
def fibonacci(n: Int): Int = {
  def recur(n: Int, a: Int, b: Int): Int =
    if (n == 0) a
    else recur(n - 1, b, a + b)
  recur(n, 0, 1)
}

object MyModuleWithFactorial {
  def abs(n: Int) =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    def recur(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else recur(n - 1, n * acc)

    recur(n, 1)
  }

  private def formatResult(name: String, x: Int, f: Int => Int): String = {
    val msg = "The %d of %d is %d"
    msg.format(name, x, f(x))
  }

  private def formatAbs(i: Int) =
    formatResult("absolute value", i, abs)

  private def formatFactorial(i: Int) =
    formatResult("factorial", i, factorial)


  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
}

// Parametric polymorphism
def findFirst[A](as: Array[A], p: A => Boolean): Int = {
  def recur(n: Int): Int =
    if (n >= as.length) -1
    else if (p(as(n))) n
    else recur(n + 1)

  recur(0)
}

def findFirst [Q] (qs: Iterable[Q], predicate: Q => Boolean): Option[Q] = {
  for (q <- qs) {
    if (predicate(q)) return Some(q)
  }
  None
}

// Exercise 2.2
def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
  def recur(a: Int, b: Int): Boolean = {
    if (b >= as.length) true
    else if (!ordered(as(a), as(b))) false
    else recur(b, b + 1)
  }

  recur(0, 1)
}

isSorted[Int](Array(1, 2, 2), (x, y) => x <= y)

object increasing extends Function2[Int, Int, Boolean] {
  def apply(a: Int, b: Int): Boolean = a < b
}
isSorted(Array(1,2,3), increasing)
increasing.apply(1, 2)
increasing(1, 2)

def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
  b => f(a, b)

//Exercise 2.3
def curry[A,B,C](f: (A, B) => C): A => (B => C) =
  a => partial1(a, f)
//Exercise 2.4
def uncurry[A,B,C](f: A => B => C): (A, B) => C =
  (a, b) => f(a)(b)
//Exercise 2.5
def compose[A,B,C](f: B => C, g: A => B): A => C =
  a => f(g(a))