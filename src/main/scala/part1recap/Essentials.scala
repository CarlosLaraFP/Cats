package part1recap

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

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

  // for comprehensions (syntactic sugar to compress chains of map/flatMap)
  val checkerBoard: List[(Int, Char)] = List(1, 2, 3).flatMap(n => List('a', 'b', 'c').map(c => (n, c))) // Cartesian product
  // equivalent expression (collapsed by the compiler into chains of map/flatMap, available to any data structure that supports them)
  val anotherCheckerBoard: List[(Int, Char)] = for {
    n <- List(1, 2, 3)
    c <- List('a', 'b', 'c')
  } yield (n, c)

  // options and try
  // calling the apply method of the companion object of the Option abstract class
  val anOption: Option[Int] = Option(3) // returns Some( ) or None (subtypes of Option)
  // Option avoids defensive null-checking coding
  val doubledOption: Option[Int] = anOption.map(_ * 2) // works with either Some or None

  // Try companion object apply method takes in an expression that might throw
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
  // Futures need an implicit ExecutionContext (data structure that holds the mechanism for scheduling threads)
  // With Scala 2.13, the below implementation changed to save Future spawning overhead.
  // import scala.concurrent.ExecutionContext.Implicits.global
  // Since nested Futures will not run in parallel unless we use the blocking API, we define our own ExecutionContext.

  // Spawns ExecutionContext (starts thread pool which will serve as a platform for this ExecutionContext)
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val future: Future[Int] = Future {
    //
    42
  }

  // wait for completion (async) through a partial function (decomposed through pattern matching)
  future.onComplete {
    // this block is called a partial function
    case Success(value) => println(s"The async meaning of life is $value")
    case Failure(exception) => println(s"Meaning of value failed: $exception")
  }

  // map a Future
  val anotherFuture: Future[Int] = future.map(_ + 1) // Future(43) when it completes

  // partial functions (based on pattern matching)
  val partialFunction: PartialFunction[Int, Int] = {
    // these are the only values this function can return (anything else => match error)
    case 1 => 43
    case 3 => 56
    case 100 => 999
  }

  // more advanced stuff (higher kinded types)
  trait HigherKindedType[F[_]]

  trait SequenceChecker[F[_]] {
    def isSequential: Boolean
  }

  val listChecker: SequenceChecker[List] = new SequenceChecker[List] {
    override def isSequential = true
  }

  def main(args: Array[String]): Unit = {

  }
}
