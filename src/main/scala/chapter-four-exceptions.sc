
def failingFn(i: Int): Int = {
  val y: Int = throw new Exception("fail!")
  try {
    val x = 42 + 5
    x + y
  }
  catch {
    case e: Exception => 43
  }
}

def failingFnInlineY(i: Int): Int = {
  try {
    val x = 42 + 5
    x + ((throw new Exception("fail!")): Int)
  }
  catch {
    case e: Exception => 43
  }
}


failingFnInlineY(5)
try {
  failingFn(5)
} catch {
  case e: Exception => e.getMessage
}

// Option type
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]

  def flatMap[B](f: A => Option[B]): Option[B]

  def getOrElse[B >: A](default: => B): B

  def orElse[B >: A](ob: => Option[B]): Option[B]

  def filter(f: A => Boolean): Option[A]
}

case class Some[+A](get: A) extends Option[A] {
  override def map[B](f: A => B) = Some(f(this.get))

  override def flatMap[B](f: A => Option[B]) = f(this.get)

  override def getOrElse[B >: A](default: => B) = this.get

  override def orElse[B >: A](ob: => Option[B]) = this

  override def filter(f: A => Boolean) = if (f(this.get)) this else None
}

case object None extends Option[Nothing] {
  override def map[B](f: Nothing => B) = None

  override def flatMap[B](f: Nothing => Option[B]) = None

  override def getOrElse[B >: Nothing](default: => B) = default

  override def orElse[B >: Nothing](ob: => Option[B]) = ob

  override def filter(f: Nothing => Boolean) = None
}

def mean(xs: Seq[Double]): Option[Double] =
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)

val doubles = Seq[Double](1, 2, 3, 4, 5)
val doubles2 = Seq[Double](0, 2, 3, 4, 6)

mean(doubles)
mean(doubles2)

// Exercise 4.2
def variance(xs: Seq[Double]): Option[Double] =
  mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

variance(doubles)
variance(doubles2)

def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
val absO: Option[Double] => Option[Double] = lift(math.abs)

def insuranceRateQuote(a: Int, b: Int): Int = 5

def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String):
Option[Double] = {
  val optAge: Option[Int] = Try(age.toInt)
  val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
  map2(optAge, optTickets)(insuranceRateQuote)
  new Some(2)
}

def Try[A](a: => A): Option[A] =
  try Some(a)
  catch {
    case e: Exception => None
  }

def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
  (a, b) match {
    case (None, _) => None
    case (_, None) => None
    case (Some(x), Some(y)) => Some(f(x, y))
  }
}

def map2_b[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
  a match {
    case None => None
    case Some(aa) => b.map(bb => f(aa, bb))
  }
}

def map2_c[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a flatMap (aa => b map (bb => f(aa, bb)))

map2(Some[Int](1), None)((a: Int, b: Int) => a + b)

// Exercise 4.4
def sequence[A](a: List[Option[A]]): Option[List[A]] = {
  a.foldLeft(Some(Nil): Option[List[A]])((acc: Option[List[A]], a: Option[A]) => {
    a match {
      case None => None
      case Some(x) => acc.map((l: List[A]) => x :: l)
    }
  })
}

//Worksheet needs a lot of type hints...
val s: List[Option[Int]] = Some(get = 1) :: Some(get = 2) :: Some(get = 3) :: Nil
sequence[Int](s)

def parseInts(a: List[String]): Option[List[Int]] =
  sequence(a map (i => Try(i.toInt)))

def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
  a.foldLeft(Some(Nil): Option[List[B]])((acc, a) => f(a) match {
    case None => None
    case Some(b) => acc.map((l: List[B]) => b :: l)
  })

def traverse_b[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
  a.foldLeft(Some(Nil): Option[List[B]])((acc, a) => f(a) flatMap (b => acc map (l => b :: l)))


def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(a => a)
def sequence2_b[A](a: List[Option[A]]): Option[List[A]] = traverse_b(a)(a => a)

sequence2(s)
sequence2_b(s)

// Comprehensions
def map2_comp[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  for {
    aa <- a
    bb <- b
  } yield f(aa, bb)

// (each step is a `.flatMap` ... apart from the final one which is a `.map`..
// creates some nested bindings
// identical to a.flatMap(aa => b.map(bb => f(aa, bb)))

sealed trait Either[+E, +A] {
  def map[B] (f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E, B >: A] (b: => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C] (b: Either[EE, B]) (f: (A, B) => C): Either[EE, C]
}

case class Left[+E](value: E) extends Either[E, Nothing] {
  override def map[B](f: Nothing => B) = this
  override def flatMap[EE >: E, B](f: Nothing => Either[EE, B]) = this
  override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]) = b
  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C) =
    this
}

case class Right[+A](value: A) extends Either[Nothing, A] {
  override def map[B](f: A => B) = Right(f(value))
  override def flatMap[EE >: Nothing, B](f: A => Either[EE, B]) = f(value)
  override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]) = this
  override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C) =
    this.flatMap(aa => b.map(bb => f(aa, bb)))
}

def TryE[A] (a: => A): Either[Exception, A] = try Right(a) catch { case e: Exception => Left(e)}

def mean(xs: IndexedSeq[Double]): Either[String, Double] =
  if (xs.isEmpty)
    Left("mean of empty list!")
  else
    Right(xs.sum / xs.length)

def safeDiv(x: Int, y: Int): Either[Exception, Int] =
  try Right(x / y)
  catch { case e: Exception => Left(e) }

def traverseE[E, A, B](l: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
  l.foldLeft (Right(Nil): Either[E, List[B]]) ((acc: Either[E, List[B]], a: A) =>
  for {
    l <- acc
    b <- f(a)
  } yield b :: l)

def sequence[E, A](l: List[Either[E, A]]): Either[E, List[A]] =
  traverseE (l) (x => x)

