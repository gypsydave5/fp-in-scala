package chapter_three

import chapter_three.datastructures.{Branch, Cons, Leaf, List, Nil, Tree}

object Exercises {
  // 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  // 3.3
  def setHead[A](a: A, as: List[A]): List[A] = as match {
    case Nil => throw new Error("Cannot set head of empty list")
    case Cons(_, t) => Cons(a, t)
  }

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => if (n == 1) t else drop(t, n - 1)
  }

  // 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  // 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  // 3.9
  def length[A](as: List[A]): Int = List.foldRight(as)(0)((a, b) => 1 + b)

  // 3.10
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t)(f(z, h))(f)
  }

  // 3.11
  def sum_fl(as: List[Int]): Int = foldLeft(as)(0)(_ + _)

  def product_fl(as: List[Double]): Double = foldLeft(as)(0.0)(_ * _)

  def length_fl[A](as: List[A]): Int = foldLeft(as)(0)((l, _) => 1 + l)

  // 3.12
  def reverse[A](as: List[A]): List[A] = foldLeft(as)(Nil: List[A])((b, a) => Cons(a, b))

  // 3.13
  def fl_in_fr[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    List.foldRight(as)((b: B) => b)((a, g) => (b: B) => g(f(b, a)))(z)

  // taking an example input of List(1,2,3) and some function f
  // we can build up the function starting with the identity \b b
  // (lambda calculus like notation cribbed from Haskell)
  //
  // \b b
  // \b1 (\b b f b1 3)
  // \b2 ((\b1 (\b b f b1 1) f b2 2)
  // \b3 ((\b2 ((\b1 (\b b f b1 3) f b2 2)) f b3 1)
  // \b3 ((\b2 ((\b1 (\b b f b1 3) f b2 2)) f b3 1) 1
  // (\b2 ((\b1 (\b b f b1 3) f b2 2)) (f 1 1)
  // (\b1 (\b b f b1 3) (f (f 1 1) 2)
  // (\b b (f (f (f 1 1) 2) 3))
  // (f (f (f 1 1) 2) 3) - 1 is evaluated first - the order of application has been reversed

  def fr_in_fl[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldLeft(as)((b: B) => b)((g, a) => b => g(f(a, b)))(z)

  // same again - the technique will always reverse application order

  // 3.14
  def append[A](a1: List[A], a2: List[A]): List[A] =
    List.foldRight(a1)(a2)((x, acc) => Cons(x, acc))

  // 3.15
  def concat[A](aas: List[List[A]]): List[A] =
    foldLeft(aas)(Nil: List[A])((acc, a) => append(acc, a))

  // 3.16

  def incall(l: List[Int]): List[Int] =
    List.foldRight(l)(Nil:List[Int])((i, acc) => Cons(i + 1, acc))

  // 3.18
  def map[A, B](l: List[A], f: A => B): List[B] =
    List.foldRight(l)(Nil: List[B])((a, acc) => Cons(f(a), acc))

  // 3.18
  def filter[A](l: List[A], p: A => Boolean): List[A] =
    List.foldRight(l)(Nil: List[A])((a, acc) => if (p(a)) Cons(a, acc) else acc)

  // 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    List.foldRight(as)(Nil: List[B])((a, acc) => append(f(a), acc))

  // 3.23
  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
    def inner(as: List[A], bs: List[B], result: List[C]): List[C] =
      as match {
       case Nil => result
       case Cons(ah, at) => bs match {
         case Nil => result
           case Cons(bh, bt) => inner(at, bt, Cons(f(ah, bh), result))
       }
      }

    reverse(inner(as, bs, Nil))
  }

  //???
  def fold[A,B,C](as: List[A], bs: List[B])(z: C)(f: (C, A, B) => C): C = {
    def inner(as: List[A], bs: List[B], result: C): C =
      as match {
        case Nil => result
        case Cons(ah, at) => bs match {
          case Nil => result
          case Cons(bh, bt) => inner(at, bt, f(result, ah, bh))
        }
      }

    inner(as, bs, z)
  }

  // 3.24
  def startsWith[A](sup: List[A], sub: List[A]): Boolean =
    sub match {
      case Nil => true
      case Cons(subh, subt) => sup match {
        case Nil => false
        case Cons(suph, supt) => if (suph == subh) startsWith(supt, subt) else false
      }
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    sup match {
      case Nil => false
      case Cons(_, supt) => if (startsWith(sup, sub)) true else hasSubsequence(supt, sub)
    }
  }

  // 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  // 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  // 3.27
  def depth[A](t: Tree[A]): Int = {

  }
}