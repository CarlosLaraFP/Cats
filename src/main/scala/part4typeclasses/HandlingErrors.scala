package part4typeclasses

import cats.Monad

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try


object HandlingErrors {

  // JVM exception type (Throwable), or any other type that represents an error

  trait MyMonadError[M[_], E] extends Monad[M] {
    def raiseError[A](e: E): M[A]
  }

  import cats.MonadError
  import cats.instances.either._ // implicit MonadError

  type ErrorOr[A] = Either[String, A]
  val monadErrorEither: MonadError[ErrorOr, String] = MonadError[ErrorOr, String]

  val success: ErrorOr[Int] = monadErrorEither.pure(23) // Right(32)
  val failure: ErrorOr[Int] = monadErrorEither.raiseError("Something wrong") // Left("Something wrong")
  // "recover"
  val handledError: ErrorOr[Int] = monadErrorEither.handleError(failure) {
    case "Badness" => 44
    case _ => 89
  }
  // "recoverWith"
  val handledErrorWith: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure) {
    case "Badness" => monadErrorEither.pure(44)
    case _ => Left("Something else")
  }
  // "filter" turns a value into an error type
  val filteredSuccess: ErrorOr[Int] = monadErrorEither.ensure(success)("Number too small")(_ > 100)

  // Try  and Future
  import cats.instances.try_._ // implicit MonadError[Try], E == Throwable

  val exception = new RuntimeException("Really bad")
  val pureException: Try[Int] = MonadError[Try, Throwable].raiseError(exception) // Failure(exception)

  import cats.instances.future._
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  // Future which will complete with a Failure(exception)
  MonadError[Future, Throwable].raiseError(exception)


  def main(args: Array[String]): Unit = {

  }
}
