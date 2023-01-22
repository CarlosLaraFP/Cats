package part4typeclasses

import cats.Monoid
import cats.syntax._
import cats.instances.int._ // Monoid[Int]

object Folding {

  // TODO: Implement all in terms of foldLeft & foldRight
  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] = list.foldRight(List.empty[B]) {
      (currentElement, currentList) => f(currentElement) :: currentList
    }
    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list.foldRight(List.empty[B]) {
      (currentElement, currentList) => f(currentElement) ::: currentList
    }
    def filter[A](list: List[A])(predicate: A => Boolean): List[A] = list.foldRight(List.empty[A]) {
      case (currentElement, currentList) if predicate(currentElement) => currentElement :: currentList
      case (_, currentList) => currentList
    }
    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A = list.foldLeft(monoid.empty)(monoid.combine)
  }

  import cats.Eval
  import cats.Foldable

  import cats.instances.list._ // implicit Foldable[List]
  val six: Int = Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _) // 6

  import cats.instances.option._ // implicit Foldable[Option]
  val optionInt: Int = Foldable[Option].foldLeft(Option(2), 30)(_ + _) // 32

  // foldRight with Eval guarantees tail recursion, regardless of the implementation of the container
  // makes any container stack-safe by chaining Evals
  val sum: Eval[Int] = Foldable[List].foldRight(List(1, 2, 3), Eval.now(0)) {
    (num, eval) => eval.map(_ + num)
  } // .value

  val anotherSum: Int = Foldable[List].combineAll(List(1, 2, 3)) // implicit Monoid[Int]

  import cats.instances.string._
  val mappedConcat: String = Foldable[List].foldMap(List(1, 2, 3))(_.toString) // implicit Monoid[String]

  val intNested: List[Vector[Int]] = List(Vector(1, 2, 3), Vector(4, 5, 6))
  import cats.instances.vector._
  val listVector: Int = (Foldable[List] compose Foldable[Vector]).combineAll(intNested)


  def main(args: Array[String]): Unit = {
    //
    val testList = List(5, 10, 15)
    println(ListExercises.map(testList)(_ * 2))
    println(ListExercises.flatMap(testList)(a => List(a, a, a)))
    println(ListExercises.filter(testList)(_ > 5))
    println(ListExercises.combineAll(testList))
    println(six)
    println(optionInt)
    println(sum.value)
    println(listVector)
  }
}
