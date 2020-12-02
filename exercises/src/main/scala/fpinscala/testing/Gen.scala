package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.testing

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop {
    (m, n, rng) => run(m, n, rng) match {
      case Passed | Proved => p.run(m, n, rng)
      case f => f
    }
  }

  def ||(p: Prop): Prop = Prop {
    (m, n, rng) => run(m, n, rng) match {
      case Passed => Passed
      case Proved => Proved
      case _ => p.run(m, n, rng)
    }
  }

}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified = false
}

case class Falsified(failure: FailedCase,
                     successes: SuccessCount) extends Result {
  override def isFalsified = true
}

case object Proved extends Result {
  override def isFalsified: Boolean = false
}

object Prop {

  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
  type MaxSize = Int

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop =
    Prop { (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
    }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  val S: Gen[ExecutorService] = weighted(
    choose(1,4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case (s, a) => f(a)(s).get }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  def main(args: Array[String]): Unit = {

    val es: ExecutorService = Executors.newCachedThreadPool

    val smallInt = Gen.choose(-10,10)
    val maxProp = forAll(listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }

    val sortProp = forAll(listOf(smallInt)) { ns =>
      val sorted = ns.sorted
      sorted.sliding(2).foldRight(true) {
        case (one :: two :: Nil, b) => one <= two && b
        case (_, b) => b
      }
    }

    def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = Par.map2(p, p2)(_ == _)

    val p2 = checkPar {
      equal(
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2)
      )
    }

    run(p2)

  }

}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeLessThan(stopExclusive - start)).map(_ - start))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.int).map(_ % 2 == 0))

  def double: Gen[Double] = Gen(State(RNG.double))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val w1 = g1._2 / (g1._2 + g2._2)
    double.flatMap(d => if (d < w1) g1._1 else g2._1)
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(listOfN(_, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(n max 1, g))

  object ** {
    def unapply[A, B](p: (A, B)): Option[(A, B)] = Some(p)
  }

}

case class Gen[+A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] =
    flatMap(a => unit(f(a)))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f andThen (_.sample)))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(Gen.listOfN(_, this))

  def unsized: SGen[A] = SGen(_ => this)

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    for {
      a <- this
      b <- g
    } yield f(a, b)

  def **[B](g: Gen[B]): Gen[(A,B)] = (this map2 g)((_,_))

}

case class SGen[+A](forSize: Int => Gen[A])

