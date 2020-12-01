package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    map(int)(i => if (i < 0) -(i + 1) else i)(rng)

  def double(rng: RNG): (Double, RNG) =
    map(nonNegativeInt)(_.toDouble / Int.MaxValue)(rng)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, s1) = int(rng)
    val (d, s2) = double(s1)
    ((i, d), s2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, s1) = double(rng)
    val (i, s2) = int(s1)
    ((d, i), s2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, s1) = double(rng)
    val (d2, s2) = double(rng)
    val (d3, s3) = double(rng)
    ((d1, d2, d3), s3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    (1 to count).foldRight[(List[Int], RNG)]((Nil, rng)) {
      case (_, (lst, r)) =>
        val (i, s) = int(r)
        (i :: lst, s)
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, s1) = ra(rng)
      val (b, s2) = rb(s1)
      (f(a, b), s2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight[Rand[List[A]]](unit(Nil))(map2(_, _)(_ :: _))

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r1) = f(rng)
    g(a)(r1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else nonNegativeLessThan(n)
    })

  def _map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(f andThen unit)

  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => {
      _map(rb)(b => {
        f(a, b)
      })
    })

}

case class State[S, +A](run: S => (A, S)) {

  import fpinscala.state.State._

  def map[B](f: A => B): State[S, B] =
    flatMap(f andThen unit)

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    for {
      a <- this
      b <- sb
    } yield f(a, b)

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State((a, _))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight[State[S, List[A]]](unit(Nil))(_.map2(_)(_ :: _))

  def _get[S]: State[S, S] = State(s => (s, s))

  def _set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- _get
      _ <- _set(f(s))
    } yield ()

  def update(in: Input): Machine => Machine = m => (in, m) match {
    case (Coin, Machine(true, candies, coins)) if candies > 0 =>
      Machine(locked = false, candies, coins + 1)
    case (Turn, Machine(false, candies, coins)) =>
      Machine(locked = true, candies - 1, coins)
    case (Coin, Machine(_, _, coins)) => m.copy(coins = coins + 1)
    case _ => m
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    sequence(inputs.map(update _ andThen modify)).flatMap(_ => State {
      case s@Machine(_, candies, coins) => ((coins, candies), s)
    })

}
