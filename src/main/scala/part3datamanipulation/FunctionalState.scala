package part3datamanipulation

import cats.Eval
import cats.data.IndexedStateT

object FunctionalState {

  /*
    Cats State is a data structure that describes the evolution of a system.
    State is a purely functional abstraction for iterative computation.

    Define a custom domain model with case classes,
    implement custom flatMap/map using Monad type class,
    then use State to transform.
  */

  type MyState[S, A] = S => (S, A)
  // wraps a function and runs a single computation
  import cats.data.State

  // the State of the system is of type Int and the output of every single computation is a String
  val incrementState: State[Int, String] = State(currentInt => (currentInt + 1, s"Incremented $currentInt"))

  // State.run returns an instance of Eval
  val (eleven, incremented) = incrementState.run(10).value

  // iterative anti-pattern
  var a: Int = 10
  a += 1
  val firstComputation: String = s"Added 1 to 10, obtained $a"
  a *= 5
  val secondComputation: String = s"Multiplied with 5, obtained $a"

  // pure FP with States
  val firstTransformation: State[Int, String] = State((state: Int) => (state + 1, s"Added 1 to 10, obtained ${state + 1}"))
  val secondTransformation: State[Int, String] = State((state: Int) => (state * 5, s"Multiplied with 5, obtained ${state * 5}"))
  // function composition
  val compositeTransformation: State[Int, (String, String)] = firstTransformation flatMap {
    firstResult => secondTransformation map {
      secondResult => (firstResult, secondResult)
    }
  }

  val compositeTransformationFor: State[Int, (String, String)] = for {
    firstResult <- firstTransformation
    secondResult <- secondTransformation
  } yield (firstResult, secondResult)

  val func1: Int => (Int, String) = (state: Int) => (state + 1, s"Added 1 to 10, obtained ${state + 1}")
  val func2: Int => (Int, String) = (state: Int) => (state * 5, s"Multiplied with 5, obtained ${state * 5}")

  val compositeFunc: Int => (String, (Int, String)) = func1 andThen {
    case (newState, firstResult) => (firstResult, func2(newState))
  }

  val (a1, (s, a2)) = compositeFunc(10)
  // State's conveniently keeps the state value at the top of the hierarchy without nesting

  case class ShoppingCart(items: List[String], total: Double)

  def addToCart(item: String, price: Double): State[ShoppingCart, Double] = State {
    /*
      Transition/transformation between an old ShoppingCart to another ShoppingCart and the total amount at that point.
    */
    (cart: ShoppingCart) => (ShoppingCart(item :: cart.items, price + cart.total), price + cart.total)
  }

  val testCart: State[ShoppingCart, Double] = for {
    _ <- addToCart("Sword", 10.50)
    _ <- addToCart("Shield", 11.50)
    total <- addToCart("Arrow", 12.50)
  } yield total

  def inspect[A, B](f: A => B): State[A, B] = State { (s: A) => (s, f(s)) }

  def get[A]: State[A, A] = State { (s: A) => (s, s) }

  def set[A](value: A): State[A, Unit] = State { (_: A) => (value, ()) }

  def modify[A](f: A => A): State[A, Unit] = State { (s: A) => (f(s), ()) }

  // Daniel: You have a great ability to think in abstract generic terms

  // all methods above already available
  import cats.data.State._

  // this is a state transition between an int and a final result
  // within a for-comprehension, .run is called in every method to extract the State value at each step
  // the methods that return Unit are labeled with underscore
  // purely functional using immutability (program.run(initial) runs each line in sequence passing the previous)
  // imperative program reduced to pure functional programming (no vars, loops)
  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 43)
    c <- inspect[Int, Int](_ * 2)
  } yield (a, b, c)


  def main(args: Array[String]): Unit = {
    //
    println(compositeTransformation.run(10).value)
    println(compositeTransformationFor.run(10).value)
    println(compositeFunc(10))
    val initialCart = ShoppingCart(List("Shirt"), 10.50)
    val state = this.addToCart("Pants", 19.50)
    println(state.run(initialCart).value)
    println(testCart.run(ShoppingCart(Nil, 0.0)).value)
    println(this.inspect[Int, String]((i: Int) => s"The number is $i").run(100).value)
    println(this.get[Int].run(100).value)
    println(this.set[Int](50).run(100).value)
    println(this.modify[String]((_: String) => "Scala").run("C++").value)
    println(program.run(0).value)
  }
}
