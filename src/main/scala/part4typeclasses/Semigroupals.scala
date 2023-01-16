package part4typeclasses

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Semigroupals {

  trait MySemigroupal[F[_]] {
    // tuples the values inside a generic container and returns the tuple inside the same container type
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = ???
  }

  import cats.Semigroupal
  import cats.instances.option._ // implicit Semigroupal[Option]

  val optionSemigroupal: Semigroupal[Option] = Semigroupal[Option]
  val aTupledOption: Option[(Int, String)] = optionSemigroupal.product(Some(123), Some("a string")) // Some((123, "a string"))
  val aNoneTupled: Option[(Int, Nothing)] = optionSemigroupal.product(Some(123), None) // None

  import cats.instances.future._ // implicit Semigroupal[Future]
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  // both Futures run in parallel and when both complete, their values are tupled
  val aTupledFuture: Future[(String, Int)] = Semigroupal[Future].product(Future("the meaning of life"), Future(42))

  // Why are Semigroupals useful??

  import cats.instances.list._

  val aTupledList: List[(Int, String)] = Semigroupal[List].product(List(1, 2), List("a", "b")) // Cartesian product


  def main(args: Array[String]): Unit = {
    //
    println(aTupledList)
  }
}
