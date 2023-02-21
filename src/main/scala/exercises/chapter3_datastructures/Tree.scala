package exercises.chapter3_datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  /*
  * Exercise 3.25
  Write a function size that counts the number of nodes
  (leaves and branches) in a tree.
  */
  def size[A](t: Tree[A]): Int = {
    def loop(t: Tree[A]): Int = {
      t match {
        case Leaf(_) => 1
        case Branch(left, right) => 1 + loop(left) + loop(right)
      }
    }
    loop(t)
  }

  /*
  Exercise 3.26
  Write a function maximum that returns the maximum element in a Tree[Int].
   (Note: In Scala, you can use x.max(y) or x max y to compute the maximum of
    two integers x and y.)
   */
  def maximum(t: Tree[Int]): Int = {
    def loop(t: Tree[Int]): Int = {
      t match {
        case Leaf(v) => v
        case Branch(left, right) =>  loop(left).max(loop(right))
      }
    }

    loop(t)
  }

  /*
  * Exercise 3.27
  Write a function depth that returns the maximum path length from the root of a tree to any leaf.
  * */
  def depth[A](t: Tree[A]): Int = {
    def loop(t: Tree[A]): Int = {
      t match {
        case Leaf(_) => 0
        case Branch(left, right) => (loop(left) max loop(right)) + 1
      }
    }

    loop(t)
  }

  /*
   Exercise 3.28
  Write a function map, analogous to the method of the same name on List,
  that modifies each element in a tree with a given function.
  */
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    def loop(t: Tree[A]): Tree[B] = {
      t match {
        case Leaf(v) => Leaf(f(v))
        case Branch(left, right) => Branch(loop(left), loop(right))
      }
    }

    loop(t)
  }

  /*
  Exercise 3.29
  Generalize size, maximum, depth, and map, writing a new function fold that abstracts
   over their similarities. Reimplement them in terms of this more general function.
   Can you draw an analogy between this fold function and the left and right folds for List?
  * */

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(v => 1)((fr, fl) => 1 + fr + fl)

  def maximumViaFold(t: Tree[Int]): Int = fold(t)(v => v)((fl, fr) =>  fl max fr)

  def depthViaFold[A](t: Tree[A]): Int = fold(t)(v => 0)((fl, fr) =>  (fl max fr) + 1)

  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)( (v: A) => (Leaf(f(v)): Tree[B]) )((fl: Tree[B], fr: Tree[B]) =>  Branch(fl, fr): Tree[B])

}

object TestTree extends App {
  val testTree1 = Branch(Leaf(5), Branch(Leaf(2), Leaf(7)))
  val testTree2 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Branch(Leaf(4), Leaf(6))), Leaf(9)))

  println("\nExercise 3.25")
  println("size: " + Tree.size(testTree1))
  println("size: " + Tree.size(testTree2))

  println("\nExercise 3.26")
  println("maximum: " + Tree.maximum(testTree1))
  println("maximum: " + Tree.maximum(testTree2))

  println("\nExercise 3.27")
  println("depth: " + Tree.depth(testTree1))
  println("depth: " + Tree.depth(testTree2))

  println("\nExercise 3.28")
  println("map: " + Tree.map(testTree1)(_ + 1))
  println("map: " + Tree.map(testTree2)(_ + 1))

  println("\nExercise 3.29")
  println("sizeViaFold: " + Tree.sizeViaFold(testTree1))
  println("sizeViaFold: " + Tree.sizeViaFold(testTree2))

  println("maximumViaFold: " + Tree.maximumViaFold(testTree1))
  println("maximumViaFold: " + Tree.maximumViaFold(testTree2))

  println("depthViaFold: " + Tree.depthViaFold(testTree1))
  println("depthViaFold: " + Tree.depthViaFold(testTree2))

  println("mapViaFold: " + Tree.mapViaFold(testTree1)(_ + 1))
  println("mapViaFold: " + Tree.mapViaFold(testTree2)(_ + 1))
}


