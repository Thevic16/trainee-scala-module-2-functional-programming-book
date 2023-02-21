package exercises.chapter4_errorhandling

sealed trait Option[+A] {
  /*
  Exercise 4.1
  Implement all of the preceding functions on Option. As you implement each function,
  try to think about what it means and in what situations you’d use it. We’ll explore
  when to use each of these functions next. Here are a few hints for solving this
  exercise:

  * It’s fine to use pattern matching, though you should be able to implement all
  the functions besides map and getOrElse without resorting to pattern matching.

  * For map and flatMap, the type signature should be enough to determine the
   implementation.

  * getOrElse returns the result inside the Some case of the Option, or if the
   Option is None, returns the given default value.

  * orElse returns the first Option if it’s defined; otherwise, it returns
   the second Option.
  */
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(get: A) => Some(f(get))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(get) => get
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(_) => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(get) if(f(get)) => this
    case _ => None
  }

  /*
  Exercise 4.3
  Write a generic function map2 that combines two Option values using a binary function.
   If either Option value is None, then the return value is too. Here is its signature:
  */

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (None, None) | (None, _) | (_, None) => None
    case (Some(va), Some(vb)) => Some(f(va, vb))
  }

  def map2ViaFlatMap[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(va => b.map(vb => f(va, vb)))

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /*
  Exercise 4.2
  Implement the variance function in terms of flatMap. If the mean of a sequence is m,
   the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
   See the definition of variance on Wikipedia (http://mng.bz/0Qsr).
  */
  def variance(xs: Seq[Double]): Option[Double] = {
    val m: Option[Double] = mean(xs)

    m match {
      case None => None
      case Some(m) => mean(xs.map((x: Double) => math.pow(x - m, 2)))
    }
  }

  // Solution
  def varianceViaFlatMap(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  /*
  def varianceViaForExpression(xs: Seq[Double]): Option[Double] = {

    val ms: Seq[Double] = for {
      m <- mean(xs)
      x <- xs
    } yield math.pow(x - m, 2)

    mean(ms)
  }
  */



  /*
  Exercise 4.4
  Write a function sequence that combines a list of Options into one Option
  containing a list of all the Some values in the original list. If the original
   list contains None even once, the result of the function should be None;
    otherwise the result should be Some with a list of all the values.
    Here is its signature:
  * */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h.flatMap(vh => sequence(t).map(vh :: _))
    }

  /*
   Exercise 4.5
  Implement this function. It’s straightforward to do using map and sequence, but try for a more efficient
   implementation that only looks at the list once. In fact, implement sequence in terms of traverse.
  */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => f(h).flatMap(hb => traverse(t)(f).map(tb => hb :: tb))
    }
}

object TestOption extends App {
  val seq: Seq[Double] = Seq(1,2,3,4,5,6,7,8,9,10)
  println("\n Exercise 4.2")
  println("variance: "+ Option.variance(seq))
  println("varianceSolution: "+ Option.varianceViaFlatMap(seq))

  println("\n Exercise 4.2")
  val a: List[Option[Int]] = List(Some(1), Some(2), Some(3), Some(4), Some(5))
  val a1: List[Option[Int]] = List(Some(1), Some(2), Some(3), Some(4), Some(5), None)
  println("sequence: " + Option.sequence(a))
  println("sequence: " + Option.sequence(a1))

  println("\n Exercise 4.2")
  val a3: List[Int] = List(1,2,3,4,5,6,7,8,9,10)
  println("traverse: "+ Option.traverse(a3)(Some(_)))
  println("traverse: "+ Option.traverse(a3)(x => if (x % 2 == 0) Some(x) else None))
  println("traverse: "+ Option.traverse(Nil)(Some(_)))
}
