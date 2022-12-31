package part3datamanipulation

import cats.Eval
import cats.data.IndexedStateT

object FunctionalState {

  /*
    Cats State is a data structure that describes the evolution of a system.
    State is a purely functional abstraction for iterative computation.
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

  val compositeTransformation: State[Int, (String, String)] = firstTransformation flatMap {
    firstResult => secondTransformation map {
      secondResult => (firstResult, secondResult)
    }
  }

  val compositeTransformationFor: State[Int, (String, String)] = for {
    firstResult <- firstTransformation
    secondResult <- secondTransformation
  } yield (firstResult, secondResult)


  def main(args: Array[String]): Unit = {
    //
    println(compositeTransformation.run(10).value)
    println(compositeTransformationFor.run(10).value)
  }
}
