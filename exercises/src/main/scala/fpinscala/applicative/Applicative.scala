package fpinscala
package applicative

import monads.Functor
import state._
import StateUtil._
import monoids._

import language.implicitConversions

trait Applicative[F[_]] extends Functor[F] {

  self =>

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)(_ (_))

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(map(fa)(f.curried))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] =
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
        (self.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))
    }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fa, fb)(G.map2(_, _)(f))
    }

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldRight(unit(Map[K, V]())) { case ((key, fValue), fMap) =>
      map2(fMap, fValue)((map, value) => map + (key -> value))
    }

}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {

  self =>

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))

  def compose[G[_]](G: Monad[G]): Monad[({type f[x] = F[G[x]]})#f] =
    new Monad[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
      override def join[A](mma: F[G[F[G[A]]]]): F[G[A]] = ??? // impossible to implement
    }

}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = Id(f(value).value)
}

object Monad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
    new Monad[({type f[x] = Either[E, x]})#f] {
      override def unit[A](a: => A): Either[E, A] = Right(a)
      override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
        ma.flatMap(f)
    }

  def stateMonad[S]: Monad[({type f[x] = State[S, x]})#f] =
    new Monad[({type f[x] = State[S, x]})#f] {
      override def unit[A](a: => A): State[S, A] = State(s => (a, s))
      override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
        st flatMap f
    }

  def composeM[F[_], N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]): Monad[({type f[x] = F[N[x]]})#f] =
    new Monad[({type f[x] = F[N[x]]})#f] {
      override def unit[A](a: => A): F[N[A]] = F.unit(N.unit(a))
      override def join[A](mma: F[N[F[N[A]]]]): F[N[A]] =
        F.flatMap(mma)(nfna => F.map(T.sequence(nfna))(N.join))
    }

  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
  }

}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  def apply[F[_]: Applicative]: Applicative[F] = implicitly[Applicative[F]]

  val streamApplicative: Applicative[LazyList] = new Applicative[LazyList] {

    def unit[A](a: => A): LazyList[A] =
      LazyList.continually(a) // The infinite, constant stream

    override def map2[A, B, C](a: LazyList[A], b: LazyList[B]) // Combine elements pointwise
                              (f: (A, B) => C): LazyList[C] =
      a zip b map f.tupled

    // sequence in this case represents a stream of lists consisting of
    // the elements of the same index from the initial streams

  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] =
    new Applicative[({type f[x] = Validation[E, x]})#f] {
      override def unit[A](a: => A): Validation[E, A] = Success(a)

      override def apply[A, B](fab: Validation[E, A => B])(fa: Validation[E, A]): Validation[E, B] =
        (fab, fa) match {
          case (Success(f), Success(a)) => Success(f(a))
          case (Success(_), f@Failure(_, _)) => f
          case (f@Failure(_, _), Success(_)) => f
          case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ (h2 +: t2))
        }
    }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[({type f[x] = Const[M, x]})#f] =
    new Applicative[({type f[x] = Const[M, x]})#f] {
      override def unit[A](a: => A): M = M.zero
      override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {

  self =>

  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_] : Applicative, A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(identity)

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse(fa)(a => Id(f(a)))(Monad.idMonad).value

  import Applicative._

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B, x]})#f, A, Nothing](as)(f)(monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _ <- set(s2)
    } yield b).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, s) => ((), f(s, a)))._2

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                            (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G.product(H))

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] =
    new Traverse[({type f[x] = F[G[x]]})#f] {
      override def traverse[H[_] : Applicative, A, B](fa: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
        self.traverse(fa)(G.traverse(_)(f))
    }

}

object Traverse {

  val listTraverse: Traverse[List] = new Traverse[List] {
    override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = {
      val G = Applicative[G]
      fa.foldRight(G.unit(List[B]()))((a, g) => G.map2(f(a), g)(_ :: _))
    }
  }

  val optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def traverse[G[_] : Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = {
      val G = Applicative[G]
      fa.fold(G.unit(Option.empty[B]))(a => G.map(f(a))(Some(_)))
    }
  }

  val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
    override def traverse[G[_] : Applicative, A, B](fa: Tree[A])(f: A => G[B]): G[Tree[B]] =
      Applicative[G].map2(
        f(fa.head),
        listTraverse.traverse(fa.tail)(this.traverse(_)(f))
      )(Tree(_, _))
  }

}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
