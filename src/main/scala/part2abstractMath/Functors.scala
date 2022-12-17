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


  def main(args: Array[String]): Unit = {

    println(incrementedNumbers)
    println(incrementedOption)
    println(incrementedTry)
    println(do10x(modifiedList))
    println(do10x(modifiedOption))
    println(do10x(modifiedTry))
  }
}
