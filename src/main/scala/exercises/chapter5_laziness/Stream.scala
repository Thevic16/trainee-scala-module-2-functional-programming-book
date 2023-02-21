package exercises.chapter5_laziness

import Stream._

import scala.annotation.tailrec
trait Stream[+A] {
  // Code write by the book
  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  /*
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }
  */

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  /*
  Exercise 5.1
  Write a function to convert a Stream to a List, which will force its evaluation and let you look
   at it in the REPL. You can convert to the regular List type in the standard library.
   You can place this and other functions that operate on a Stream inside the Stream trait.
  */

  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

  def toListViaTail: List[A] = {
    @tailrec
    def go(stream: Stream[A], acc: List[A]): List[A] = {
      stream match {
        case Empty => acc
        case Cons(h, t) => go(t(), acc :+ h())
      }
    }

    go(this, List())
  }

  /*
  Exercise 5.2
  Write the function take(n) for returning the first n elements of a Stream,
   and drop(n) for skipping the first n elements of a Stream.
  */
  def take(n: Int): Stream[A] = {
    @tailrec
    def go(stream: Stream[A], acc: Stream[A], n: Int): Stream[A] = {
      if (n >= 1) // This condition could go inside of the pattern matching as if guards
        stream match {
          case Empty => acc
          case Cons(h, t) => go(t(), cons(h(), acc), n-1)
        }
      else
        acc
    }

    go(this, empty, n)
  }

  def drop(n: Int): Stream[A] = {
  @tailrec
  def go(stream: Stream[A], n: Int): Stream[A] = {
    if (n >= 1) // I could put this condition as a pattern guard inside of match
      stream match {
        case Empty => stream
        case Cons(_, t) => go(t(), n - 1)
      }
    else
      stream
  }

  go(this, n)
}

  /*
  Exercise 5.3
  Write the function takeWhile for returning all starting elements of a Stream
  that match the given predicate.
  */
  def takeWhile(p: A => Boolean): Stream[A] = {
    @tailrec
    def go(stream: Stream[A], acc: Stream[A]): Stream[A] = {
      stream match {
        case Cons(h, t) if p(h()) => go(t(), cons(h(), acc))
        case _ => acc
      }

    }
    go(this, empty)
  }

  /*
  Exercise 5.4
  Implement forAll, which checks that all elements in the Stream match a given predicate.
  Your implementation should terminate the traversal as soon as it encounters a nonmatching value.
  */
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  /*
  Exercise 5.5
  Use foldRight to implement takeWhile.
  */
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => {
      if(p(h)) cons(h, t)
      else empty
    })

  /*
  Exercise 5.6
  Hard: Implement headOption using foldRight.
  */
  def headOption: Option[A] = foldRight(None: Option[A])((h,_) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  /*
  Exercise 5.7
  Implement map, filter, append, and flatMap using foldRight.
  The append method should be non-strict in its argument.
  */
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => {
      if (p(h)) cons(h, t)
      else t
    })

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => {
      if (t == empty[A]) cons(h, s)
      else cons(h, t)
    })

  /*
  // Better way to do it
  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => cons(h,t))
  */

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  /*
  Exercise 5.13
  Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and zipAll.
  The zipAll function should continue the traversal as long as either stream has more elements—it
   uses Option to indicate whether each stream has been exhausted.
  */
  def mapViaUnFold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some(f(h()), t())
    case Empty => None
  }

  def takeViaUnFold(n: Int): Stream[A] = unfold((this, n)){
    case (Cons(h ,t), n) if n >= 1 => Some(h(), (t(), n-1))
    case _ => None
  }

  def takeWhileViaUnFold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)){
      case (Cons(h1 ,t1), Cons(h2 ,t2)) => Some(f(h1(),h2()), (t1(), t2()))
      case _ => None
    }

  // This is the solution because I didnt understand well what I have to code.
  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  /*
  Exercise 5.14
  Hard: Implement startsWith using functions you’ve written. It should check if one
   Stream is a prefix of another. For instance, Stream(1,2,3) startsWith Stream(1,2)
    would be true.
  */
  def startsWith[B](s: Stream[B]): Boolean = {
    val compareStream = zipWith(s)(_ == _)
    compareStream.foldRight(true)(_ && _)
  }

  /*
  Exercise 5.15
  Implement tails using unfold. For a given Stream, tails returns the Stream of suffixes
   of the input sequence, starting with the original Stream. For example, given Stream(1,2,3),
    it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()).
  */
  def tails: Stream[Stream[A]] = unfold(this){
    case stream @ Cons(h, t) => Some(stream, t())
    case Empty => None
  }

  def hasSubsequence[A](s2: Stream[A]): Boolean =
    this.tails.exists(s1 => s1.startsWith(s2))

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  // Code write by the book
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

  /*
  Exercise 5.8
  Generalize ones slightly to the function constant,
  which returns an infinite Stream of a given value.
  */
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  /*
  // This is more efficient than `cons(a, constant(a))` since it's just
  // one object referencing itself.
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }
  */

  /*
  Exercise 5.9
  Write a function that generates an infinite stream of integers, starting from n,
  then n + 1, n + 2, and so on.[7]

  In Scala, the Int type is a 32-bit signed integer, so this stream will switch
  from positive to negative values at some point, and will repeat itself after
   about four billion elements.
  */
  def from(n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }

  /*
  Exercise 5.10
  Write a function fibs that generates the infinite stream of
   Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
  */
  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))

    go(0, 1)
  }

  /*
  Exercise 5.11
  Write a more general stream-building function called unfold. It takes an initial state,
   and a function for producing both the next state and the next value in the generated stream.
  */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  /*
   Exercise 5.12
  Write fibs, from, constant, and ones in terms of unfold.
  */
  val fibsViaUnFold = {
    unfold((0,1))(i => Some(i._1 , (i._2, i._1 + i._2)))
  }

  def fromViaUnFold(n: Int): Stream[Int] = {
    unfold(n)(n => Some(n , n + 1))
  }

  def constantViaUnFold[A](a: A): Stream[A] = unfold(a)(a => Some(a, a))

  val onesViaUnFold = {
    unfold(1)(one => Some(one , one))
  }
}

object TestStream extends App {

  def printStream[A](stream: Stream[A]): Unit = stream match {
    case Empty => ()
    case Cons(h, t) => println(h()) ; printStream(t())
  }

  val stream1: Stream[Int] = Stream(1,2,3,4,5,6,7,8,9,10)
  println("\nExercise 5.1")
  println("toList: " + stream1.toList)
  println("toListViaTail: " + stream1.toListViaTail)

  println("\nExercise 5.2")
  val take5 = stream1.take(5)
  println("take: " + take5)
  printStream(take5)

  val drop5 = stream1.drop(5)
  println("drop: " + drop5)
  printStream(drop5)

  println("\nExercise 5.3")
  val takeWhileEven = stream1.takeWhile(_ < 5)
  println("takeWhileEven: " + takeWhileEven)
  printStream(takeWhileEven)

  println("\nExercise 5.4")
  println("forAll: "+ stream1.forAll(_ < 100))
  println("forAll: "+ stream1.forAll(_ % 2 == 0))

  println("\nExercise 5.7")
  val mapPlusTwo = stream1.map(_ + 2)
  println("map: "+ mapPlusTwo)
  printStream(mapPlusTwo)

  val filterViaFold = stream1.filter(_ % 2 == 0)
  println("filter: "+ filterViaFold)
  printStream(filterViaFold)

  val appendViaFold = stream1.append(Stream(11,12,13,14,15,16))
  println("append: " + appendViaFold)
  printStream(appendViaFold)

  val flatMapViaFold = stream1.flatMap(x => Stream(x + 2))
  println("flatMap: " + flatMapViaFold)
  printStream(flatMapViaFold)

  val exampleBook = Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0)
  println("exampleBook: " + exampleBook.toList)
  //printStream(exampleBook)

  println("\nExercise 5.9")
  val from = Stream.from(1)
  println("from: "+ from.take(5).toList)

  println("\nExercise 5.12")
  println("fibs: "+ Stream.fibsViaUnFold.take(7).toList)
  println("fromViaUnFold: "+ fromViaUnFold(1).take(5).toList)

  println("\nExercise 5.13")
  val mapPlusTwoViaUnFold = stream1.mapViaUnFold(_ + 2).toList
  println("mapViaUnFold: " + mapPlusTwoViaUnFold)

  val take5ViaUnFold = stream1.takeViaUnFold(5).toList
  println("takeViaUnFold: " + take5ViaUnFold)

  val takeWhileEvenUnFold = stream1.takeWhileViaUnFold(_ < 5).toList
  println("takeWhileEvenUnFold: " + takeWhileEvenUnFold)

  val constantsTwo = constant(2)
  val zipWithViaUnFold = stream1.zipWith(constantsTwo)(_ * _).toList
  println("zipWithViaUnFold: "+ zipWithViaUnFold)

  val stream2: Stream[Int] = Stream(1,2,3,4,5)
  val zipWithAll12 = stream1.zipWithAll(stream2)((a ,b) => a.flatMap(va => b.map(vb => va * vb))).toList
  val zipWithAll21 = stream2.zipWithAll(stream1)((a ,b) => a.flatMap(va => b.map(vb => va * vb))).toList
  println("zipWithAll12" + zipWithAll12)
  println("zipWithAll21" + zipWithAll21)

  println("\nExercise 5.14")
  val startsWith1 = Stream(1, 2 , 3).startsWith(Stream(1, 2))
  val startsWith2 = Stream(1, 2 , 3).startsWith(Stream(1, 3))
  println("startsWith1: " + startsWith1)
  println("startsWith2: " + startsWith2)

  println("\nExercise 5.15")
  val tails = for {
    stream <- Stream(1, 2, 3).tails.toList
  } yield stream.toList
  println("tails: "+ tails)

  val hasSubsequence1 = Stream(1, 2 , 3).hasSubsequence(Stream(1, 3))
  val hasSubsequence2 = Stream(1, 2 , 3).hasSubsequence(Stream(2, 3))
  val hasSubsequence3 = Stream(1, 2 , 3).hasSubsequence(Stream(3))
  val hasSubsequence4 = Stream(1, 2 , 3).hasSubsequence(Stream(1))
  val hasSubsequence5 = Stream(1, 2 , 3).hasSubsequence(Stream(1, 4))
  val hasSubsequence6 = Stream(1, 2 , 3).hasSubsequence(Stream(3, 2))
  val hasSubsequence7 = Stream(1, 2 , 3).hasSubsequence(Stream(1, 2, 3))
  println("\nhasSubsequence6")
  println(hasSubsequence1)
  println(hasSubsequence2)
  println(hasSubsequence3)
  println(hasSubsequence4)
  println(hasSubsequence5)
  println(hasSubsequence6)
  println(hasSubsequence7)
}
