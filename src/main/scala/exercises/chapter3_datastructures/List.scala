package exercises.chapter3_datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // exercises
  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, tail) => tail
    case Nil => throw new RuntimeException("Can't call tail in a empty list")
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, tail) => Cons(h, tail)
    case Nil => throw new RuntimeException("Can't call setHead with a empty list")
  }

  /*
  Again, it's somewhat subjective whether to throw an exception when asked to drop more elements than the list
  contains. The usual default for `drop` is not to throw an exception, since it's typically used in cases where this
  is not indicative of a programming error. If you pay attention to how you use `drop`, it's often in cases where the
  length of the input list is unknown, and the number of elements to be dropped is being computed from something else.
  If `drop` threw an exception, we'd have to first compute or check the length and only drop up to that many elements.
  */
  def drop[A](l: List[A], n: Int): List[A] = {
    if(n >= 1)
      l match {
      case Nil => Nil
      case Cons(_, tail) => drop(tail, n-1)
      }
    else l
  }

  def dropByCondition[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) if f(head) => dropByCondition(tail, f)
    case Cons(head, tail) if !f(head) => Cons(head, dropByCondition(tail, f))
  }

  /*
  Somewhat overkill, but to illustrate the feature we're using a _pattern guard_, to only match a `Cons` whose head
  satisfies our predicate, `f`. The syntax is to add `if <cond>` after the pattern, before the `=>`, where `<cond>` can
  use any of the variables introduced by the pattern.
  */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  /*
  Note that we're copying the entire list up until the last element. Besides being inefficient, the natural recursive
  solution will use a stack frame for each element of the list, which can lead to stack overflows for
  large lists (can you see why?). With lists, it's common to use a temporary, mutable buffer internal to the
  function (with lazy lists or streams, which we discuss in chapter 5, we don't normally do this). So long as the
  buffer is allocated internal to the function, the mutation is not observable and RT is preserved.
  Another common convention is to accumulate the output list in reverse order, then reverse it at the end, which
  doesn't require even local mutation. We'll write a reverse function later in this chapter.
  */

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new RuntimeException("Can't call init with a empty list")
    case Cons(_, tail) if tail == Nil => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]

    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => List(buf.toList: _*)
      case Cons(h, t) => buf += h; go(t)
    }

    go(l)
  }

  // Exercise 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0) ((a:A, b:Int) => b+1)

  // Exercise 3.10
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(as: List[A], acc:B): B = {
      as match {
        case Nil => acc
        case Cons(head, tail) => loop(tail, f(acc, head))
      }
    }

    loop(as, z)
  }

  // Exercise 3.11
  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)
  def product3(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)
  def length2[A](as: List[A]): Int = foldLeft(as, 0) ((b:Int, a:A) => b+1)
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]()) ((lb: List[A], a: A) => Cons(a, lb))

  // Exercise 3.14
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)((la:A, r: List[A]) => Cons(la, r))

  // Exercise 3.15
  def concat[A] (listOfList: List[List[A]]): List[A] =
    foldLeft(listOfList, List[A]())((rAcc: List[A], l: List[A]) => appendViaFoldRight(rAcc, l))

  // Exercise 3.16
  /*
  * Write a function that transforms a list of integers by adding 1 to each element.
  * (Reminder: this should be a pure function that returns a new List!)
  * */

  // Exercise 3.17
  /*
  * Write a function that turns each value in a List[Double] into a String. You can use the expression
  *  d.toString to convert some d: Double to a String.
  * */

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    @tailrec
    def loob(la: List[A], lb: List[B]): List[B] = {
      la match {
        case Nil => lb
        case Cons(head, tail) => loob(tail, Cons(f(head), lb))
      }
    }
    val lb = loob(l, Nil)
    reverse(lb)
  }

  //Exercise 3.18
  /*
  * Write a function map that generalizes modifying each element in a list while
  *  maintaining the structure of the list.
  * */
  def map2[A, B](l: List[A])(f: A => B): List[B] =
    foldLeft(l, List[B]())((lb: List[B], a: A) => Cons(f(a), lb))

  /*
  Exercise 3.19
  Write a function filter that removes elements from a list unless they satisfy a given predicate.
   Use it to remove all odd numbers from a List[Int].

  def filter[A](as: List[A])(f: A => Boolean): List[A]
   */
  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) if f(head) => filter(tail)(f)
    case Cons(head, tail) if !f(head) => Cons(head, filter(tail)(f))
  }

  /*
  Exercise 3.20
  Write a function flatMap that works like map except that the function given will return a list
   instead of a single result, and that list should be inserted into the final resulting list.
    Here is its signature:
   */
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldLeft(as, List[B]())((lb: List[B], a:A) => appendViaFoldRight(lb, f(a)))

  /*
  Exercise 3.21
  Use flatMap to implement filter.
  */
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)((a:A) => if (f(a)) Nil else List(a))

  /*
  Exercise 3.22
  Write a function that accepts two lists and constructs a new list by adding corresponding elements.
   For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
   */
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = {
    if (length(a) != length(b)) throw new RuntimeException("List a and List b should have same length")

    @tailrec
    def loop(a: List[Int], b: List[Int], acc: List[Int]): List[Int] = {
      a match {
        case Nil => acc
        case Cons(ah, at) => b match {
          case Cons(bh, bt) => loop(at, bt, Cons(ah+bh, acc))
        }
      }
    }

    val acc = loop(a, b, Nil)
    reverse(acc)
  }

  /*
  To match on multiple values, we can put the values into a pair and match on the pair, as shown next, and the same
  syntax extends to matching on N values (see sidebar "Pairs and tuples in Scala" for more about pair and tuple
  objects). You can also (somewhat less conveniently, but a bit more efficiently) nest pattern matches: on the
  right hand side of the `=>`, simply begin another `match` expression. The inner `match` will have access to all the
  variables introduced in the outer `match`.
  The discussion about stack usage from the explanation of `map` also applies here.
  */

  def addPairwise2(a: List[Int], b: List[Int]): List[Int] = {
    if (length(a) != length(b)) throw new RuntimeException("List a and List b should have same length")

    @tailrec
    def loop(a: List[Int], b: List[Int], acc: List[Int]): List[Int] = {
      (a, b) match {
        case (Nil, Nil) => acc
        case (Cons(ah, at), Cons(bh, bt) ) => loop(at, bt, Cons(ah + bh, acc))
      }
    }

    val acc = loop(a, b, Nil)
    reverse(acc)
  }

  /*
  * Exercise 3.23
  Generalize the function you just wrote so that it’s not specific to integers or addition.
  Name your generalized function zipWith.*/

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = {
    if (length(a) != length(b)) throw new RuntimeException("List a and List b should have same length")

    @tailrec
    def loop(a: List[A], b: List[B], acc: List[C]): List[C] = {
      (a, b) match {
        case (Nil, Nil) => acc
        case (Cons(ah, at), Cons(bh, bt)) => loop(at, bt, Cons(f(ah , bh), acc))
      }
    }

    val acc = loop(a, b, Nil)
    reverse(acc)
  }

  def take[A](l: List[A], n: Int): List[A] = {

    @tailrec
    def loop(l: List[A], n: Int, acc: List[A]): List[A] = {
      if (n <= 0) acc
      else l match {
        case Nil => acc
        case Cons(head, tail) => loop(tail, n-1,  Cons(head, acc))
      }
    }

    val acc = loop(l, n , Nil)
    reverse(acc)
  }


  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    val lenSub = length(sub)

    @tailrec
    def loop(sup: List[A], sub: List[A]): Boolean = {
        sup match {
        case Nil => false
        case Cons (_, supTail) => if (take (sup, lenSub) == sub) true else loop(supTail, sub)
      }
    }

    loop(sup, sub)
  }


}



object TestList extends App {

  val x0 = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }
  println(x0)

  val x1 = List(1, 2, 4) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }
  println(x1)

  val x2 = List(1, 7, 4, 5, 6) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }
  println(x2)

  val x3 = List(1, 7, 4, 5, 6)
  println(List.drop(x3, 3))
  println(List.drop(x3, 5))
  println(List.drop(x3, 6))

  val x4: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  println(List.dropByCondition(x4, (x:Int) => x % 2 == 0))
  println(List.init(x4))
  println(List.init(x3))
  println(List.init2(x4))
  println(List.init2(x3))

  val xs: List[Int] = List(1, 2, 3, 4, 5)
  val ex1 = List.dropWhile(xs, (x: Int) => x < 4)
  println(ex1)

  // Exercise 3.8
  println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))

  // Exercise 3.9
  println(List.length(xs)) // 4
  println(List.length(x4)) // 10

  // Exercise 3.10
  println("foldRight sum:" + List.foldRight(x4, 0)(_ + _))
  println("foldRight mult:" + List.foldRight(x4, 1.0)(_ * _))

  println("foldLeft sum:" + List.foldLeft(x4, 0)(_ + _))
  println("foldLeft mult:" + List.foldLeft(x4, 1.0)(_ * _))

  // Exercise 3.11
  println("\n \n Exercise 3.11")
  val x5: List[Double] = List(2, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  val x6: List[Double] = List(10, 20, 33, 44, 75, 96, 47, 48, 449, 54510)
  println("foldLeft sum3:" + List.sum3(x4))
  println("foldLeft product3:" + List.product3(x5))
  println("foldLeft length2:" + List.length2(x5))
  println("foldLeft length2:" + List.length2(x6))

  // Exercise 3.12
  println(List[Int]()) // Nil
  println("foldLeft reverse:" + List.reverse(x5))

  // Exercise 3.14
  println("appendViaFoldRight"+ List.appendViaFoldRight(x5, x6))

  // Exercise 3.15
  println("\n\n Exercise 3.15")
  val listOfList = List(List(1, 2), List(3, 4), List(5, 6), List(7, 8), List(9, 10))
  println("flatMap: "+List.concat(listOfList))

  // Exercise 3.16
  println("\n\n Exercise 3.16")
  val x7: List[Double] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  println("map: " + List.map(x7)(_ + 1))

  // Exercise 3.17
  println("\n\n Exercise 3.17")
  println("map: " + List.map(x7)(_.toString))

  // Exercise 3.18
  println("\n\n Exercise 3.18")
  println("map2: " + List.map(x7)(_ + 1))

  // Exercise 3.19
  println("\n\n Exercise 3.19")
  println("filter: " + List.filter(x7)(_ % 2 != 0))

  // Exercise 3.20
  println("\n\n Exercise 3.20")
  println("flatMap: " + List.flatMap(List(1,2,3))(i => List(i,i)))

  // Exercise 3.21
  println("\n\n Exercise 3.21")
  println("filterViaFlatMap: " + List.filterViaFlatMap(x7)(_ % 2 != 0))

  // Exercise 3.22
  println("\n\n Exercise 3.22")
  println("addPairwise: " + List.addPairwise(List(1,2,3), List(4,5,6)))
  println("addPairwise2: " + List.addPairwise2(List(1,2,3), List(4,5,6)))

  // Exercise 3.23
  println("\n\n Exercise 3.23")
  println("zipWith: " + List.zipWith(List(1,2,3), List(4,5,6))(_ + _))

  // Exercise 3.24
  println("\n\n Exercise 3.24")
  println("take: " + List.take(List(1,2,3,4,5,6,7,8,9,10), 5))
  println("take: " + List.take(List(1,2,3,4,5,6,7,8,9,10), 7))
  println("hasSubsequence: " + List.hasSubsequence(List(1,2,3,4), List(1,2))) //true
  println("hasSubsequence: " + List.hasSubsequence(List(1,2,3,4), List(2,3))) //true
  println("hasSubsequence: " + List.hasSubsequence(List(1,2,3,4), List(4))) //true

  println("hasSubsequence: " + List.hasSubsequence(List(1,2,3,4), List(1,2,4))) //false
  println("hasSubsequence: " + List.hasSubsequence(List(1,2,3,4), List(5))) //false
  println("hasSubsequence: " + List.hasSubsequence(List(1,2,3,4), List(1,2,3,4,5))) //false
  println("hasSubsequence: " + List.hasSubsequence(List(1,2,3,4), List(2,4))) //false

}
