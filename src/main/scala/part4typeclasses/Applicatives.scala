package part4typeclasses

object Applicatives {

  // Applicative is a higher kinded type class that extends Functor => map + pure methods
  import cats.Applicative

  import cats.instances.list._
  val listApplicative: Applicative[List] = Applicative[List] // fetch implicit
  val aList: List[Int] = listApplicative.pure(2) // List(2)

  import cats.instances.option._
  val optionApplicative: Applicative[Option] = Applicative[Option] // fetch implicit
  val anOption: Option[String] = optionApplicative.pure("Typelevel") // Some("Typelevel")

  // pure extension method given the presence of an implicit Applicative of the given type
  import cats.syntax.applicative._
  val aSweetList: List[Int] = 2.pure[List] // List(2)
  val aSweetOption: Option[Int] = 2.pure[Option] // Some(2)

  // Monad extends Applicative (and Applicative extends Functor)

  // Applicative is rarely used in practice because we already have the stronger Monad type class
  // Use cases are only for non-monadic types

  // A notable exception is cats.data.Validated because it does not adhere to Monad laws (Validated accumulates instead of short-circuiting)
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val aValidValue: ErrorsOr[Int] = Validated.valid(43) // "pure"
  val aModifiedValidated: ErrorsOr[Int] = aValidValue.map(_ + 1) // map from Functor

  // constructs implicit where the pure method behaves as line 28 and map as usual
  val validatedApplicative: Applicative[ErrorsOr] = Applicative[ErrorsOr] // constructs implicit

  // TODO: Implement Semigroupal's product method in terms of the Applicative's pure and map methods
  def ap[F[_], A, B](ff: F[A => B])(fa: F[A]): F[B] = ??? // assume it's already implemented
  def productWithApplicatives[F[_], A, B](fa: F[A], fb: F[B])(implicit applicative: Applicative[F]): F[(A, B)] = {
    // assume ap's ff takes A => (A, B) and returns F[(A, B)]
    val functionWrapper: F[B => (A, B)] = applicative.map(fa)(a => (b: B) => (a, b))
    ap(functionWrapper)(fb)
  }


  def main(args: Array[String]): Unit = {

  }
}
