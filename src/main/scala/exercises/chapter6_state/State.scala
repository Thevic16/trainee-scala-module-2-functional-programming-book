package exercises.chapter6_state

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
  /*
  Exercise 6.1
  Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue (inclusive).
   Make sure to handle the corner case when nextInt returns Int.MinValue, which doesn’t have a non-negative
    counterpart.
  */

  // What I Think I have to do
  /*
  def nonNegativeInt(rng: RNG): (Int, RNG) = {

    def go(rng: RNG): (Int, RNG) = {
      val (n1, rng2) = rng.nextInt
      if (n1 >= 0 && n1 <= Int.MaxValue) (n1, rng2)
      else go(rng2)
    }

      go(rng)
  }
   */

  // Solution: What I really have to do!!!
  // We need to be quite careful not to skew the generator.
  // Since `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)`,
  // it suffices to increment the negative numbers by 1 and make them positive.
  // This maps Int.MinValue to Int.MaxValue and -1 to 0.
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  /*
  Exercise 6.2
  Write a function to generate a Double between 0 and 1, not including 1.
   Note: You can use Int.MaxValue to obtain the maximum positive integer value,
    and you can use x.toDouble to convert an x: Int to a Double.
  */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    (i->d, r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (i -> d, r) = intDouble(rng)
    (d -> i, r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1,d2,d3), r3)
  }

  /*
  Exercise 6.4
  Write a function to generate a list of random integers.
  */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = {
      if(count <= 1) (acc, rng)
      else {
        val (i, r) = nonNegativeInt(rng)
        go(count-1, r, acc :+ i)
      }
    }
    go(count, rng, List())
  }


  // Code write by the book
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = rng => rng.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  /*
  Exercise 6.5
  Use map to reimplement double in a more elegant way. See exercise 6.2.
  */
  val _double: Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue + 1))

  /*
  Exercise 6.6
  Write the implementation of map2 based on the following signature.
  This function takes two actions, ra and rb, and a function f for combining their results,
   and returns a new action that combines them:
  */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a , b), rng2)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  // My solitution don't work because it use the same instance of rng and always generate the same number
  /*
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => (fs.map(s => s(rng)._1), rng)
  }

  def _ints(count: Int): Rand[List[Int]] = {
    val aListOfRand: List[Rand[Int]] = List.fill(count)(int)
    sequence(aListOfRand)
  }
   */

  // In `sequence`, the base case of the fold is a `unit` action that returns
  // the empty list. At each step in the fold, we accumulate in `acc`
  // and `f` is the current element in the list.
  // `map2(f, acc)(_ :: _)` results in a value of type `Rand[List[A]]`
  // We map over that to prepend (cons) the element onto the accumulated list.
  //
  // We are using `foldRight`. If we used `foldLeft` then the values in the
  // resulting list would appear in reverse order. It would be arguably better
  // to use `foldLeft` followed by `reverse`. What do you think?
  def sequenceSolution[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  // It's interesting that we never actually need to talk about the `RNG` value
  // in `sequence`. This is a strong hint that we could make this function
  // polymorphic in that type.
  def _intsSolution(count: Int): Rand[List[Int]] =
    sequenceSolution(List.fill(count)(int))


  /*
  Exercise 6.8
  Implement flatMap, and then use it to implement nonNegativeLessThan.
  */
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(i => {
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      unit(mod)
    else nonNegativeLessThan(n)
  })

  /*
  Exercise 6.9
  Reimplement map and map2 in terms of flatMap. The fact that this is possible is what we’re referring
   to when we say that flatMap is more powerful than map and map2.
  */
  def _map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => _map(rb)(b => f(a, b)))
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = {
    State((s: S) => {
      val (a, s1) = run(s)
      (f(a), s1)
    })
  }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State((s: S) => {
      val (a, s1) = run(s)
      val (b, s2) = sb.run(s1)
      (f(a, b), s2)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State((s: S) => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S,A](a: A): State[S, A] = State(s => (a, s))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}

object TestState extends App {
  val rng = RNG.Simple(42)
  val (n1, rng2) = rng.nextInt
  println("n1: "+n1 + " rng2: "+ rng2)
  val (n2, rng3) = rng2.nextInt
  println("n2: " + n2 + " rng3: " + rng3)

  //println(Int.MaxValue.toDouble/(Int.MaxValue.toDouble + 1))

  println("\n Exercise 6.2")
  println(RNG.intDouble(rng))
  println(RNG.doubleInt(rng))
  println(RNG.double3(rng))


  println("\n Exercise 6.4")
  println("ints: " + RNG.ints(5)(rng))
  println("ints: " + RNG.ints(10)(rng))

  println("\n Exercise 6.7")
  //println("_ints: " + RNG._ints(5)(rng))
  //println("_ints: " + RNG._ints(10)(rng))

  val _intsSolutionFive = RNG._intsSolution(5)(rng)
  println("_intsSolution: " + _intsSolutionFive)
  println("_intsSolution: " + RNG._intsSolution(10)(rng))

  println("\n Exercise 6.7")
  //val sequence = RNG.sequence(List(r =>(1, r), r =>(2, r), r =>(3, r)): List[RNG.Rand[Int]])
  //println("sequence: " + sequence(rng))

  val sequenceSolution = RNG.sequenceSolution(List(r => (1, r), r => (2, r), r => (3, r)): List[RNG.Rand[Int]])
  val sequenceSolutionWithRng = sequenceSolution(rng)
  println("sequenceSolution: " + sequenceSolutionWithRng)

  println("\n Exercise 6.8")
  println("nonNegativeLessThan: "+ RNG.nonNegativeLessThan(10)(rng))

}

