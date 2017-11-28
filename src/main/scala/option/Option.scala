package option

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

object Some {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  val absO: Option[Double] => Option[Double] = lift(math.abs)

  def insuranceRateQuote(a: Int, b: Int): Int = 5

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
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

}

object main extends App {
  val s = Some(1)::Some(2)::Some(3)::Nil
  println(s)
  println(Some.sequence(s))
}


