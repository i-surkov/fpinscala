package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps


trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    val zero = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    val zero: List[A] = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  def optionMonoid[A: Monoid]: Monoid[Option[A]] = new Monoid[Option[A]] {
    val ma: Monoid[A] = implicitly
    override def op(a1: Option[A], a2: Option[A]): Option[A] = for {
      one <- a1
      two <- a2
    } yield ma.op(one, two)
    override def zero: Option[A] = Some(ma.zero)
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1 andThen a2
    override def zero: A => A = identity
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero: A = m.zero
  }

  import fpinscala.testing._
  import Prop._
  import Gen._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val op = m.op _
    val zero = m.zero
    forAll(listOfN(3, gen)) {
      case a :: b :: c :: Nil => op(op(a, b), c) == op(a, op(b, c))
      case _ => false
    } && forAll(gen)(a => op(a, zero) == a && op(zero, a) == a)
  }

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(a => b => f(a, b))(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(b => a => f(a, b))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.isEmpty)
      m.zero
    else if (as.length == 1)
      f(as.head)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    type M = Option[(Int, Int)]

    // Doesn't follow the laws but whatever
    val mm = new Monoid[M] {
      override def op(a1: M, a2: M): M = (a1, a2) match {
          case (Some((min1, max1)), Some((min2, max2))) if min2 >= max1 =>
            Some((min1, max2))
          case _ => None
        }
      override def zero: M = Some((Int.MaxValue, Int.MinValue))
    }
    foldMapV(ints, mm)(a => Some((a, a))).isDefined
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] =
      a1.map2(a2)(m.op)
    override def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(v, par(m))(a => Par.lazyUnit(f(a)))

  def isEmpty(s: String): Int = if (s.isEmpty) 0 else 1

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
      case (Stub(s1), Part(ls, w, rs)) => Part(s1 + ls, w, rs)
      case (Part(ls, w, rs), Stub(s2)) => Part(ls, w, rs + s2)
      case (Part(ls1, w1, rs1), Part(ls2, w2, rs2)) =>
        Part(ls1, w1 + w2 + isEmpty(rs1 + ls2), rs2)
    }

    override def zero: WC = Stub("")
  }

  def count(s: String): Int = foldMapV(s, wcMonoid)(c =>
    if (c == ' ') Part("", 0, "") else Stub(c.toString)
  ) match {
    case Stub(s) => isEmpty(s)
    case Part(l, w, r) => w + isEmpty(l) +isEmpty(r)
  }

  def main(args: Array[String]): Unit = {
    println(count(" my test never lie s"))
  }

  def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      override def op(a1: (A, B), a2: (A, B)): (A, B) = (a1, a2) match {
        case ((a1, b1), (a2, b2)) => (ma.op(a1, a2), mb.op(b1, b2))
      }

      override def zero: (A, B) = (ma.zero, mb.zero)
    }

  def functionMonoid[A, B](mb: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      override def op(a1: A => B, a2: A => B): A => B = a =>
        mb.op(a1(a), a2(a))

      override def zero: A => B = _ => mb.zero
    }

  def mapMergeMonoid[K, V](mv: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero: Map[K, V] = Map[K, V]()
      def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, mv.op(a.getOrElse(k, mv.zero),
            b.getOrElse(k, mv.zero)))
        }
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))
    
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(a => (b: B) => f(a, b))(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldMap(as)(identity)(m)

  def toList[A](as: F[A]): List[A] = foldRight(as)(List[A]())(_ :: _)

}

object ListFoldable extends Foldable[List] {

  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def toList[A](as: List[A]): List[A] = as

}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {

  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

}

object StreamFoldable extends Foldable[LazyList] {

  override def foldRight[A, B](as: LazyList[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: LazyList[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {

  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Leaf(a) => f(a)
      case Branch(left, right) =>
        mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
    }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
    as match {
      case Leaf(a) => f(z, a)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
    as match {
      case Leaf(a) => f(a, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }

}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Some(a) => f(a)
      case None => mb.zero
    }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as match {
      case Some(a) => f(z, a)
      case None => z
    }

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
    as match {
      case Some(a) => f(a, z)
      case None => z
    }
}

