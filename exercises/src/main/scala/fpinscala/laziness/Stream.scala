package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def toList: List[A] = foldRight(List[A]())(_ :: _)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((a, b) => if (p(a)) cons(a, b) else empty)

  def headOption: Option[A] = foldRight[Option[A]](None)((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](empty)((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](empty)((a, b) => f(a).append(b))

  def map2[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    }

  def take2(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), n) if n > 0 => Some(h(), (t(), n - 1))
      case _ => None
    }

  def takeWhile3(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h, t), _) => Some((Some(h()), None), (t(), empty))
      case (_, Cons(h, t)) => Some((None, Some(h())), (empty, t()))
      case _ => None
    }

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile3(_._2.isDefined).forAll(x => x._1 == x._2)

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case s@Cons(_, t) => Some(s, t())
      case _ => None
    }.append(empty)

  def hasSubsequence[B >: A](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight[(Stream[B], B)]((cons(z, empty), z)) {
      case (a, (s, acc)) =>
        lazy val b = f(a, acc)
        (cons(b, s), b)
    }._1

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

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

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  val fibs: Stream[Int] = {
    def fibs0(one: Int, two: Int): Stream[Int] =
      cons(one + two, fibs0(two, one + two))
    cons(0, cons(1, fibs0(0, 1)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }

  val fibs2: Stream[Int] = unfold((0, 1)) {
    case (one, two) => Some(one, (two, one + two))
  }

  def from2(n: Int): Stream[Int] = unfold(n)(i => Some(i, i + 1))

  def constant2[A](a: A): Stream[A] = unfold(a)(s => Some(s, s))

  val ones2: Stream[Int] = constant(1)

}