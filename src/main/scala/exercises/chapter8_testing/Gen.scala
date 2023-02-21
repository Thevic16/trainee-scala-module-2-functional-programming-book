package exercises.chapter8_testing

import exercises.chapter6_state.{RNG, State}
import exercises.chapter8_testing.Gen.unit
import exercises.chapter8_testing.Prop.{Falsified, MaxSize, Passed, Result, TestCases}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

/*
Exercise 8.1
To get used to thinking about testing in this way, come up with properties that specify the implementation
of a sum: List[Int] => Int function. You don’t have to write your properties down as executable
ScalaCheck code—an informal description is fine. Here are some ideas to get you started:

* Reversing a list and summing it should give the same result as summing the original, nonreversed list.
* What should the sum be if all elements of the list are the same value?
* Can you think of other properties?

My answers:
* Mess up the list and summing, it should have the same result as summing the original.
* If we divide the list into two part and summing these part, should be the same as summing up the original one.

Solution:
Here are a few properties:

* The sum of the empty list is 0.
* The sum of a list whose elements are all equal to `x` is just the list's length multiplied by `x`. We might express this as `sum(List.fill(n)(x)) == n*x`
* For any list, `l`, `sum(l) == sum(l.reverse)`, since addition is commutative.
* Given a list, `List(x,y,z,p,q)`, `sum(List(x,y,z,p,q)) == sum(List(x,y)) + sum(List(z,p,q))`, since addition is associative. More generally, we can partition a list into two subsequences whose sum is equal to the sum of the overall list.
* The sum of 1,2,3...n is `n*(n+1)/2`.

Exercise 8.2
What properties specify a function that finds the maximum of a List[Int]?
* Mess up the list and find the maximum, should be equal as find the maximum in the original.
* Reversing the list and find the maximum should be the same as find the maximum in the original list.
* what is the maximum of a empty list? None or error for me.

Solution:
* The max of a single element list is equal to that element.
* The max of a list is greater than or equal to all elements of the list.
* The max of a list is an element of that list.
* The max of the empty list is unspecified and should throw an error or return `None`.

*/


case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  //def check: Boolean

  /*
  Exercise 8.3
  Assuming the following representation of Prop, implement && as a method of Prop.
  */
  /*
  def &&(p: Prop): Prop = new Prop {
    def check = Prop.this.check && p.check
  }
   */

  /*
  Exercise 8.9
  Now that we have a representation of Prop, implement && and || for composing Prop values.
  Notice that in the case of failure we don’t know which property was responsible,
  the left or the right. Can you devise a way of handling this, perhaps by allowing
  Prop values to be assigned a tag or label which gets displayed in the event of a failure?
  */
  // solution
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Passed => p.run(max, n, rng)
        case x => x
      }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        // In case of failure, run the other prop.
        case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
        case x => x
      }
  }

  /* This is rather simplistic - in the event of failure, we simply prepend
   * the given message on a newline in front of the existing message.
   */
  def tag(msg: String) = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
  }

}

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  // Implementation book.
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }



}

object Gen {
  /*
  Exercise 8.4
  Implement Gen.choose using this representation of Gen. It should generate integers in the range start
   to stopExclusive. Feel free to use functions you’ve already written.
  */
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
  }

  /*
  Exercise 8.5
  Let’s see what else we can implement using this representation of Gen.
   Try implementing unit, boolean, and listOfN.
  */
  def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))
  def boolean: Gen[Boolean] = choose(1, 3).map((i:Int) => if(i == 1) true else false)
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {

    def go(l: List[Gen[A]]): Gen[List[A]] = {
      l match {
        case Nil => Gen(State.unit(Nil))
        case h :: t  => h.flatMap((hh: A) => go(t).map((tt: List[A]) => hh :: tt) )
      }
    }

      go(List.fill(n)(g))
  }

  /*
  Exercise 8.7
  Implement union, for combining two generators of the same type into one,
   by pulling values from each generator with equal likelihood.
  */
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)

  /*
  Exercise 8.12
  Implement a listOf combinator that doesn’t accept an explicit size. It should return
   an SGen instead of a Gen. The implementation should generate lists of the requested size.
  */
  def listOf[A](g: Gen[A]): SGen[List[A]] = {
        SGen((forSize: Int) => g.listOfNViaFlatMap(forSize))
  }

  /*
  Exercise 8.13
  Define listOf1 for generating nonempty lists, and then update your specification of max to use
   this generator.
  */
  //solution
  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfNViaFlatMap(n max 1))

}


case class Gen[+A](sample: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(a => f(a)))

  /*
  Exercise 8.6
  Implement flatMap, and then use it to implement this more dynamic version of listOfN.
  Put flatMap and listOfN in the Gen class.
  */
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))
  def listOfNViaFlatMap(n: Int): Gen[List[A]] = Gen.listOfN(n, this) // Actually this function use flatMat inside

  /*
  Exercise 8.10
  Implement helper functions for converting Gen to SGen. You can add this as a method on Gen.
  */
  def unsized: SGen[A] = SGen(_ => this)
}

case class SGen[+A](forSize: Int => Gen[A]) {

}

object TestGen extends App {
  val rng1 = RNG.Simple(42)
  val rng2 = RNG.Simple(16)
  val rng3 = RNG.Simple(1999)
  val rng4 = RNG.Simple(3)
  val gen1 = Gen(State(RNG.nonNegativeInt))
  val gen2 = Gen(State(RNG.double))

  println("\nExercise 8.4")
  println("choose (1  to 10): "+ Gen.choose(1, 10).sample.run(rng1))
  println("choose (1  to 10): "+ Gen.choose(1, 10).sample.run(rng2))
  println("choose (1  to 10): "+ Gen.choose(1, 10).sample.run(rng3))
  println("choose (1  to 10): "+ Gen.choose(1, 10).sample.run(rng4))

  println("\nExercise 8.5")
  println("unit: "+ Gen.unit(5).sample.run(rng1))
  println("unit: "+ Gen.unit(10).sample.run(rng3))

  println("\nboolean: "+ Gen.boolean.sample.run(rng1))
  println("boolean: "+ Gen.boolean.sample.run(rng2))
  println("boolean: "+ Gen.boolean.sample.run(rng3))
  println("boolean: "+ Gen.boolean.sample.run(rng4))

  println("\nlistOfN: "+ Gen.listOfN(10,gen1).sample.run(rng1))
  println("\nlistOfN: "+ Gen.listOfN(10,gen1).sample.run(rng2))
  println("\nlistOfN: "+ Gen.listOfN(10,gen1).sample.run(rng3))
  println("\nlistOfN: "+ Gen.listOfN(10,gen1).sample.run(rng4))

  println("\nunion: "+ Gen.union(gen1, gen2).sample.run(rng1))
  println("union: "+ Gen.union(gen1, gen2).sample.run(rng2))
  println("union: "+ Gen.union(gen1, gen2).sample.run(rng3))
  println("union: "+ Gen.union(gen1, gen2).sample.run(rng4))
}
