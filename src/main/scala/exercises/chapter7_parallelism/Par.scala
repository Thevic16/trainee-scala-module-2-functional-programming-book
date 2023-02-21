package exercises.chapter7_parallelism

package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions

/*
Exercise 7.1
Par.map2 is a new higher-order function for combining the result of two parallel computations.
 What is its signature? Give the most general signature possible (don’t assume it works only for Int).

 def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]
*/

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /*
  Exercise 7.4
  This API already enables a rich set of operations. Here’s a simple example: using lazyUnit,
   write a function to convert any function A => B to one that evaluates its result asynchronously.
  */
  def asyncF[A,B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  /*
  Exercise 7.5
  Hard: Write this function, called sequence. No additional primitives are required. Do not call run.
  */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps match {
    case Nil => lazyUnit(Nil)
    case h :: t => flatMap(h)(vh => map(fork(sequence(t)))(vt => vh :: vt))
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  /*
  Exercise 7.6
  Implement parFilter, which filters elements of a list in parallel.
  */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val ps: Par[List[A]] = parMap(as)(a => a)
    map(ps)(l => l.filter(f))
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /*
  Exercise 7.11
  Implement choiceN and then choice in terms of choiceN.
  */
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
  {
    es =>
      val getN: Int = run(es)(n).get
      run(es)(choices(getN))
  }

  /*
  Exercise 7.13
  Implement this new primitive chooser, and then use it to implement choice and choiceN.
  */
  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    {
      es =>
        val a = run(es)(pa).get
        run(es)(choices(a))
    }

  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = chooser(cond) {
    case false => f
    case true => t
  }

  def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = chooser(n)(choices(_))

  // The function chooser, which comes up often in functional libraries, is usually called bind or flatMap:
  def flatMap[A, B](p: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val k = run(es)(p).get
      run(es)(choices(k))
    }

  /*
  Exercise 7.14
  Implement join. Can you see how to implement flatMap using join? And can you implement join using flatMap?
  */
  def join[A](paPa: Par[Par[A]]): Par[A] =
    es => {
      val pa = run(es)(paPa).get
      run(es)(pa)
    }

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }
}

object TestPar extends App {
  import Par._
  println("\n Exercise 7.4")
  val ps: List[Par[Int]] = List.fill(10)(unit(1))
  val resultSequence: Par[List[Int]] = sequence(ps)
  //println(resultSequence)
}
