package part3datamanipulation

object Evaluation {

  /*
    Cats makes the distinction between 3 forms of evaluating an expression:
    1. Evaluating an expression eagerly
    2. Evaluating lazily and every time you request it
    3. Evaluating lazily and keeping the value (memoizing)
  */
  import cats.Eval

  val instantEval: Eval[Int] = Eval.now {
    println("Computing now")
    1
  }

  // recompute block of code every time it's requested
  val redoEval: Eval[Int] = Eval.always {
    println("Computing always")
    2
  }

  // calling this more than once only runs the block of code inside the first time; subsequent = return value cached
  val delayedEval: Eval[Int] = Eval.later {
    println("Computing later")
    3
  }

  val composedEval: Eval[Int] = instantEval.flatMap(a => delayedEval.map(b => a + b))

  // identical to above
  val composedEvalFor: Eval[Int] = for {
    a <- instantEval
    b <- delayedEval
  } yield a + b

  val evalExample: Eval[Int] = for {
    a <- delayedEval
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d


  def main(args: Array[String]): Unit = {
    //
    //println(instantEval.value)
    //println(redoEval.value)
    //println(delayedEval.value)
    //println(delayedEval.value)
    //println(composedEval.value)
    //println(composedEval.value)
    println(evalExample.value) // 8
    println(evalExample.value) // 8 with only computing always prints
  }
}
