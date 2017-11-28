// non strict functions
def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
  if (cond) onTrue() else onFalse()

// OH REALLY? A THUNK? PISS OFF.

def if3[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
  if (cond) onTrue else onFalse

def maybeTwice(b: Boolean, i: => Int) =
  if (b) i + i else 0

maybeTwice(true, {
  println("hi"); 1 + 41
})

def maybeTwice2(b: Boolean, i: => Int) = {
  lazy val j = i
  if (b) j + j else 0
}

maybeTwice2(true, {
  println("hi"); 1 + 41
})

sealed trait Stream[+A] {
  def toList: List[A]
  def take(n: Int): Stream[A]
  def drop(n: Int): Stream[A]
  def takeWhile(p: A => Boolean): Stream[A]
}

case object Empty extends Stream[Nothing] {
  override def toList = Nil
  override def take(n: Int) = this
  override def drop(n: Int) = this
  override def takeWhile(p: Nothing => Boolean) = this
}

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def toList = h() :: t().toList
  override def take(n: Int) =
    if (n == 0) Stream.empty
    else Cons(h, () => t().take(n - 1))
  override def drop(n: Int) =
    if (n == 0) t()
    else t().drop(n - 1)
  override def takeWhile(p: A => Boolean) ={
    if (p(h())) Cons(h, () => t().takeWhile(p))
    else Empty
  }
}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}

val stream = Stream.apply(1,2,3,4,5,6)
var list = stream.toList
