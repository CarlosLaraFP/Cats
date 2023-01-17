package part4typeclasses

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Semigroupals {

  /*
    Semigroupal is a higher-kinded type class that can tuple elements
    (product method) without needing to wrap/unwrap.

    Monads extend Semigroupals because product is implemented in terms of flatMap/map

    Some Semigroupals are useful without being Monads (i.e. Validated)

    Semigroupal tuples and Semigroup combines (they are distinct).
  */

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

  import cats.instances.list._ // Monad[List]

  val aTupledList: List[(Int, String)] = Semigroupal[List].product(List(1, 2), List("a", "b")) // Cartesian product

  // TODO: Implement with Monad
  import cats.Monad
  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] = {
    monad.flatMap(fa)(a => monad.map(fb)(b => (a, b)))
  }

  import cats.syntax.functor._ // map
  import cats.syntax.flatMap._ // flatMap
  def cleanerProduct[F[_] : Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] = for {
    a <- fa
    b <- fb
  } yield (a, b)

  // MONADS EXTEND SEMIGROUPALS

  // use case: Validated
  import cats.data.Validated

  type ErrorsOr[T] = Validated[List[String], T]

  // implements the product of two Validated instances in terms of independently combining the error type and value type through Semigroup
  val validatedSemigroupal: Semigroupal[ErrorsOr] = Semigroupal[ErrorsOr] // requires implicit Semigroup[List[_]]

  val invalidsCombination: ErrorsOr[(Nothing, Nothing)] = validatedSemigroupal.product(
    Validated.invalid(List("Something wrong", "Wrong again")),
    Validated.invalid(List("This can't be right"))
  )

  // ***

  type EitherErrorsOr[T] = Either[List[String], T]

  import cats.instances.either._ // implicit Monad[Either]

  val eitherSemigroupal: Semigroupal[EitherErrorsOr] = Semigroupal[EitherErrorsOr]

  // implemented in terms of map/flatMap
  val eitherCombination: EitherErrorsOr[(Nothing, Nothing)] = eitherSemigroupal.product(
    Left(List("Something wrong", "Wrong again")),
    Left(List("This can't be right"))
  )
  // Implementing error tracking with Monads is not good because we lose information

  // TODO: define a Semigroupal[List] which does a zip
  val zipSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa zip fb
  }
  val zippedList: List[(Int, String)] = zipSemigroupal.product(List(1, 2), List("a", "b"))

  def main(args: Array[String]): Unit = {
    //
    println(aTupledList)
    println(productWithMonads(List(1, 2), List("a", "b")))
    println(cleanerProduct(List(1, 2), List("a", "b")))
    println(invalidsCombination)
    // Second Either is missing because flatMap/map short-circuits (prevents propagation)
    println(eitherCombination)
    println(zippedList)
  }
}
