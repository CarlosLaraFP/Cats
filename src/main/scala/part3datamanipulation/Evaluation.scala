package part3datamanipulation

import scala.annotation.tailrec

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

  val doNotRecompute: Eval[Int] = redoEval.memoize

  val tutorial: Eval[String] = Eval
    .always { println("Step 1"); "Draw sword" }
    .map { step1 => println("Step 2"); s"$step1 then raise it" }
    .memoize // remember value up to this point
    .map { steps12 => println("Step 3"); s"$steps12 then draw energy"}

  // Implement defer such that defer(Eval.now) does not run the side effects
  def defer[T](eval: => Eval[T]): Eval[T] = Eval.later(()).flatMap(_ => eval)
    //Eval later { eval.value }
  // passing eval by name (By-name parameters are evaluated every time they are used. They won’t be evaluated at all if they are unused.)

  def reverseList[T](list: List[T]): List[T] = {
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head
  }

  def reverseEvalBasic[T](list: List[T]): Eval[List[T]] = Eval later {
    //
    @tailrec
    def reverseHelper(current: List[T], result: List[T]): List[T] = current.headOption match {
      case None => result
      case Some(x) => reverseHelper(current.tail, x :: result)
    }
    reverseHelper(list, Nil)
  }

  //@tailrec
  def reverseEval[T](list: List[T]): Eval[List[T]] = {
    // Even though this looks head recursive, Eval will evaluate it tail recursively!
    if (list.isEmpty) Eval.now(list)
    else Eval.defer(reverseEval(list.tail).map(_ :+ list.head))
  }
  // Amazing. We can write stack recursive methods, wrap them in Eval.defer, and produce a tail recursive implementation.


  def main(args: Array[String]): Unit = {
    //
    println(defer(Eval.now {
      println("Now") // this should not get printed
      42
    }).value)

    val list = List(1, 2, 3)
    println(reverseEval(list).value)
  }
}
