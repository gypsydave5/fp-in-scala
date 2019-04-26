package chapter_two
// This is an example module
/* multiline comment */
/** This is the documentation for this module
  * apparently
  *
  * Well anyway, I don't make the rules so...
  */
object MyModule { // singleton object - aka module
  /*
  Everything is an object
  Objects can have zero or more members
  Objects that namespace members are called _modules_
  Method members are declared with def
  More objects are declared with val or object
   */
  def abs(n: Int): Int = // function body is a single expression
    if (n < 0) -n
    else n

  def factorial(n:Int): Int = {
    //named inner function scoped to outer
    @annotation.tailrec
    def helper(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else helper(n - 1, n * acc)

    helper(n, 1)
  }

  // abstraction of formatFunctionName
  def formatResult(name: String, n: Int, f: Int => Int) =
    "The %s of %d is %d.".format(name, n, f(n))

  private def formatAbs(x: Int) = { // function body is a block
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  // MyModule.main(Array()) to execute in the REPL
  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
    println(formatFactorial(7))
}

/*
Compile with `scalac MyModule.scala`

Execute with `scala MyModule`

or just run with the interpreter `scala MyModule.scala`
*/




