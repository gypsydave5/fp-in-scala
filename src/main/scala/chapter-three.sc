sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def fill[A](n: Int, a: A): List[A] =
    (1 until n).foldLeft[List[A]](Nil)((l, _) => Cons[A](a, l))
}

val list: List[Int] = List(1: Int, 2, 3, 4, 5 : Int)
val p: Int = List.sum(list: List[Int])
val q = List(1.0,2.0,3.0)
val r = List.product(q)
val fiveBobs = List.fill(5, "BOB")

// Exercise 3.2
def tail[A](list: List[A]): List[A] = list match {
  case Nil => Nil
  case Cons(_, t) => t
}
tail(list)

//Exercise 3.3
def setHead[A](h: A, list: List[A]): List[A] = Cons(h, tail(list))
setHead("JOE", fiveBobs)

//Exercise 3.4
def drop[A](n: Int, list: List[A]): List[A] = list match {
  case Nil => Nil
  case Cons(_, xs) => if (n == 0) list else drop(n - 1, xs)
}

drop(3, list)

def dropWhile[A](p: A => Boolean, list: List[A]): List[A] = list match {
  case Nil => Nil
  case Cons(head, tail) => if (p(head)) dropWhile(p, tail) else list
}

def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
  case Nil => a2
  case Cons(x, xs) => Cons (x, append(xs, a2))
}

//Exercise 3.6
def init[A](l: List[A]): List[A] = l match {
  case Nil => Nil
  case Cons(x, Cons(_, Nil)) => Cons(x, Nil)
  case Cons(x, xs) => Cons(x, init(xs))
}

def dropWhile2[A](p: A => Boolean)(l: List[A]): List[A] = l match {
  case Nil => Nil
  case Cons(head, tail) => if (p(head)) dropWhile2(p)(l) else l
}

def dropWhile3[A](l: List[A])(p: A => Boolean): List[A] = l match {
  case Nil => Nil
  case Cons(head, tail) => if (p(head)) dropWhile3(l)(p) else l
}

dropWhile((x: Int) => x < 2, list)
// dropWhile2((x: Int) => x < 2)(list)
//// "Type information flows from left to right..."
//dropWhile3(list)(x => x < 2)

def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
  as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)
def product2(ns: List[Int]) = foldRight(ns, 1)(_ * _)

// Exercise 3.8 - lists can be modelled as folds - see Wikipedia on lambda calculus

// Exercise 3.9
def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)
length(list)

// Exercise 3.10
def foldLeft[A, B](z: B)(as: List[A])(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(f(z, x))(xs)(f)
  }

// Exercise 3.11
def sumL(ns: List[Int]) = foldLeft(0)(ns)((x,y) => x + y)
def productL(ns: List[Int]) = foldLeft(0)(ns)(_ * _)
def lengthL(ns: List[Int]) = foldLeft(0)(ns)((acc, _) => acc + 1)

// Exercise 3.12
def reverse[A](l: List[A]): List[A] = foldLeft(Nil: List[A])(l)((acc, x) => Cons(x, acc))
val bakwdzList = reverse(list)

// Exercise 3.13
def foldLWithR[A, B](z: B)(as: List[A])(f: (B, A) => B): B =
  foldRight(as: List[A], (b: B) => b) ((a: A, acc: (B) => B) => (b) => f(acc(b), a)) (z)
val x = foldLWithR(0)(list)(_ + _)

def foldRWithL[A, B](z: B)(as: List[A])(f: (A, B) => B): B =
  foldLeft((b: B) => b) (as: List[A]) ((acc: (B) => B, a: A) => (b) => f(a, acc(b))) (z)
