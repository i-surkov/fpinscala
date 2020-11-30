package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => Cons(h, t)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n < 1) l else l match {
      case Nil => Nil
      case Cons(_, tail) => drop(tail, n - 1)
    }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, tail) if f(h) => dropWhile(tail, f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
    case Nil => Nil
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, s) => s + 1)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def reverse[A](ns: List[A]): List[A] =
    foldLeft(ns, Nil: List[A])((l, a) => Cons(a, l))

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, identity: B => B)((fb, a) => b => fb(f(a, b)))(z)

  // Using compose with a partially applied function
  // (although uglier than in Haskell)
  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight2(l, identity: B => B)((a, fb) => fb compose (f(_, a)))(z)

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight2(a1, a2)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight2(l, Nil: List[A])(append2)

  def add1(l: List[Int]): List[Int] =
    foldRight2(l, List[Int]())((a, s) => Cons(a + 1, s))

  def double2Str(l: List[Double]): List[String] =
    foldRight2(l, List[String]())((a, s) => Cons(a.toString, s))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight2(l, List[B]())((a, s) => Cons(f(a), s))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight2(as, List[A]())((a, s) => if (f(a)) Cons(a, s) else s)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight2(as, List[B]())((a, s) => append2(f(a), s))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] =
    (l1, l2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
      case _ => Nil
    }

  @tailrec
  def startsWith[A](src: List[A], trg: List[A]): Boolean =
    (src, trg) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h1, t1), Cons(h2, t2)) => h1 == h2 && startsWith(t1, t2)
    }

  @tailrec
  def hasSubsequence[A](src: List[A], trg: List[A]): Boolean =
    (src, trg) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(_, t1), _) => startsWith(src, trg) || hasSubsequence(t1, trg)
    }

}
