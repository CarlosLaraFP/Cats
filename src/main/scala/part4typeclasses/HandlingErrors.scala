package part4typeclasses

import cats.{Applicative, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object HandlingErrors {

  // JVM exception type (Throwable), or any other type that represents an error

  trait MyApplicativeError[M[_], E] extends Applicative[M] {
    def raiseError[A](e: E): M[A]
    def handleErrorWith[A](ma: M[A])(f: E => M[A]): M[A]
    def handleError[A](ma: M[A])(f: E => A): M[A] = handleErrorWith(ma)(e => pure(f(e)))
  }

  trait MyMonadError[M[_], E] extends MyApplicativeError[M, E] with Monad[M] {
    def ensure[A](ma: M[A])(error: E)(predicate: A => Boolean): M[A]
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

  // Applicatives => ApplicativeError (useful for non-monadic types, such as Validated)
  import cats.data.Validated
  import cats.instances.list._ // implicit Semigroup[List] => ApplicativeError[ErrorsOr, List[String]]
  type ErrorsOr[T] = Validated[List[String], T]
  import cats.ApplicativeError
  val applicativeError: ApplicativeError[ErrorsOr, List[String]] = ApplicativeError[ErrorsOr, List[String]]
  // pure, raiseError, handleError, handleErrorWith

  // Extension methods
  import cats.syntax.applicative._ // pure
  import cats.syntax.applicativeError._ // raiseError, handleError, handleErrorWith

  val extendedSuccess: ErrorsOr[Int] = 42.pure[ErrorsOr] // requires the implicit ApplicativeError[ErrorsOr, List[String]]
  val extendedError: ErrorsOr[Int] = List("Badness").raiseError[ErrorsOr, Int]
  val recoveredError: ErrorsOr[Int] = extendedError.recover {
    case _ => 43
  }

  import cats.syntax.monadError._ // ensure

  val testedSuccess: ErrorOr[Int] = success.ensure("Something bad")(_ > 100)


  def main(args: Array[String]): Unit = {

  }
}
