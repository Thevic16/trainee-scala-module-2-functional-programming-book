package exercises.chapter2_gettingstated

import sun.font.TrueTypeFont

object PolymorphicFunctions {

  // Exercise 2: Implement a polymorphic function to check whether
  // an `Array[A]` is sorted
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {

    @annotation.tailrec
    def go(head: A, as: Array[A]): Boolean = {
      if (as.isEmpty) true
      else if(gt(head, as.head)) false
      else go(as.head, as.tail)
    }

    go(as.head, as.tail)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  // Exercise 3: Implement `curry`.

  // Note that `=>` associates to the right, so we could
  // write the return type as `A => B => C`
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  // NB: The `Function2` trait has a `curried` method already

  // Exercise 4: Implement `uncurry`
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)
  /*
  NB: There is a method on the `Function` object in the standard library,
  `Function.uncurried` that you can use for uncurrying.
  Note that we can go back and forth between the two forms. We can curry
  and uncurry and the two forms are in some sense "the same". In FP jargon,
  we say that they are _isomorphic_ ("iso" = same; "morphe" = shape, form),
  a term we inherit from category theory.
  */

  // Exercise 5: Implement `compose`

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

}

object TestPolymorphicFunctions extends App {
  println(PolymorphicFunctions.isSorted(Array(1,2,3,4,5), (x:Int, y:Int) => x > y))
  println(PolymorphicFunctions.isSorted(Array(1,2,7,3,4,5), (x:Int, y:Int) => x > y))
  println(PolymorphicFunctions.isSorted(Array(1,2,3,4,4,5), (x:Int, y:Int) => x > y))
}

