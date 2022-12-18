package part2abstractMath

import scala.util.Try

object Functors {

  val modifiedList: List[Int] = List(1, 2, 3).map(_ + 1) // List(2, 3, 4)
  val modifiedOption: Option[Int] = Option(2).map(_ + 1) // Some(3)
  val modifiedTry: Try[Int] = Try(42).map(_ + 1) // Success(43)

  // simplified definition
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(function: A => B): F[B]
  }

  // Higher-kinded type class that generalizes the map method
  // Functors are important when we need to generalize a transformation
  import cats.Functor
  import cats.instances.list._ // includes Function[List]
  val listFunctor: Functor[List] = Functor[List]
  val incrementedNumbers: List[Int] = listFunctor.map(List(1, 2, 3))(_ + 1) // List(2, 3, 4)

  import cats.instances.option._ // includes Functor[Option
  val optionFunctor: Functor[Option] = Functor[Option]
  val incrementedOption: Option[Int] = optionFunctor.map(Option(2))(_ + 1) // Some(3)

  import cats.instances.try_._
  val incrementedTry: Try[Int] = Functor[Try].map(Try(42))(_ + 1) // Success(43)

  // generalizing an API
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)
  def do10xTry(attempt: Try[Int]): Try[Int] = attempt.map(_ * 10)

  // generic container of integers
  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)

  // define a functor for a binary tree (do not use Functor.instance)
  sealed trait Tree[+T]
  object Tree {
    // Companion object containing smart factory methods for subtypes (to compensate for type class invariance in Cats)
    // Use this approach instead of case class constructors directly
    def leaf[T](value: T): Tree[T] = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }
  // binary tree node types
  final case class Leaf[+T](value: T) extends Tree[T]
  final case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object TreeFunctor extends Functor[Tree] {
    // Recursive implementation for case Branch (not tail recursive -> will revisit during Monads)
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(value) => Leaf(f(value))
      case Branch(value, left, right) => Branch(f(value), map(left)(f), map(right)(f))
    }
  }

  // extension method for Functor
  import cats.syntax.functor._

  // TODO: Functor use cases -> Data structures that are meant to be transformed in sequence.


  def main(args: Array[String]): Unit = {
    //
    println(incrementedNumbers)
    println(incrementedOption)
    println(incrementedTry)
    println(do10x(modifiedList))
    println(do10x(modifiedOption))
    println(do10x(modifiedTry))
    // The compiler needs the explicit [Tree] type parameter because Cats type classes are invariant (i.e. Functor)
    val leaf = Tree.leaf(2)
    val mappedLeaf = do10x(leaf)
    val branch = Tree.branch(4, Tree.leaf(1), Tree.leaf(2))
    val mappedBranch = do10x(branch)
    println(mappedLeaf)
    println(mappedBranch)
    // type context bound
    //def clean10x[F[_] : Functor](container: F[Int]): F[Int] = container.map(_ * 10)
    println(leaf.map(_ * 10))
    println(branch.map(_ * 10))
  }
}
