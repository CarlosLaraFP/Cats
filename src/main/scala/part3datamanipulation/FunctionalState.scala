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

  // TODO: online store
  case class ShoppingCart(items: List[String], total: Double)

  def addToCart(item: String, price: Double): State[ShoppingCart, Double] = State {
    /*
      Transition/transformation between an old ShoppingCart to another ShoppingCart and the total amount at that point.
    */
    (state: ShoppingCart) => (ShoppingCart(item :: state.items, price + state.total), price + state.total)
  }



  def main(args: Array[String]): Unit = {
    //
    println(compositeTransformation.run(10).value)
    println(compositeTransformationFor.run(10).value)
    println(compositeFunc(10))
    val initialCart = ShoppingCart(List("Shirt"), 10.50)
    val state = addToCart("Pants", 19.50)
    println(state.run(initialCart).value)
  }
}
