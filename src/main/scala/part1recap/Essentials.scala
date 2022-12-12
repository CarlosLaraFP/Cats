package part1recap

import scala.util.Try

object Essentials {

  // values (no variables in this course due to FP immutability)
  val aBoolean: Boolean = false

  // expressions are evaluated to a value
  val ifExpression: String = if (2 > 3) "bigger" else "smaller"

  // instructions vs expressions
  // the canonical instruction is an expression returning unit (side-effects)
  val theUnit: Unit = println("Hello, Scala")

  // OOP
  class Animal
  class Cat extends Animal
  trait Carnivore {
    //
    def eat(animal: Animal): Unit
  }

  // inheritance model: extend 1 class max, but mix-in any number of traits
  class Crocodile extends Animal with Carnivore {
    //
    override def eat(animal: Animal): Unit = println("Crunch")
  }

  // singleton
  object MySingleton

  // companions (class+object or trait+object in the same file)
  object Carnivore

  // generics
  class MyList[A]

  // method notation & infix operators for single argument methods
  val three: Int = 1 + 2
  val anotherThree: Int = 1.+(2)
  // also prefix and postfix method notations

  // functional programming
  val incrementer: Int => Int = x => x + 1 // anonymous function
  val incremented: Int = incrementer(45) // 46

  // higher order functions take other functions as arguments or return other functions
  // map, flatMap, filter
  val processedList: List[Int] = List(1, 2, 3).map(incrementer) // List(2, 3, 4)
  val longerList: List[Int] = List(1, 2, 3).flatMap(i => List(i, i + 1)) // List(1, 2, 2, 3, 3, 4)

  // options and try
  // calling the apply method of the companion object of the Option abstract class
  val anOption: Option[Int] = Option(3) // returns Some( ) or None (subtypes of Option)
  // Option avoids defensive null-checking coding
  val doubledOption: Option[Int] = anOption.map(_ * 2) // works with either Some or None

  // apply method takes in an expression that might throw
  val attempt: Try[Int] = Try(42) // returns Success(42) or Failure(42) (subtypes of Try)
  val modifiedAttempt: Try[Int] = attempt.map(_ + 10)

  // pattern matching
  val unknown: Any = 45

  val ordinal: String = unknown match {
    case 1 => "first"
    case 2 => "second"
    case _ => "unknown"
  }

  val optionDescription: String = anOption match {
    case Some(value) => s"the Option is not empty: $value"
    case None => s"the Option is empty"
  }

  // Futures (data structures whose values are computed on some other thread at some point in the future)


  def main(args: Array[String]): Unit = {

  }
}
