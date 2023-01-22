package part4typeclasses

import cats.Monoid
import cats.syntax._
import cats.instances.int._

object Folding {

  // TODO: Implement all in terms of foldLeft & foldRight
  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] = list.foldRight(List.empty[B]) {
      case (currentElement, currentList) => f(currentElement) :: currentList
    }
    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list.foldRight(List.empty[B]) {
      case (currentElement, currentList) => f(currentElement) ::: currentList
    }
    def filter[A](list: List[A])(predicate: A => Boolean): List[A] = list.foldRight(List.empty[A]) {
      case (currentElement, currentList) if predicate(currentElement) => currentElement :: currentList
    }
    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A = list.foldLeft(monoid.empty)(monoid.combine)
  }


  def main(args: Array[String]): Unit = {
    //

    println(ListExercises.combineAll(List(5, 5, 5))) // 15
  }
}
