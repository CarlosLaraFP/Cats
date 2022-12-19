package part2abstractMath

object UsingMonads {

  import cats.Monad
  import cats.instances.list._
  val monadList: Monad[List] = Monad[List] // fetch implicit instance
  val simpleList: List[Int] = monadList.pure(2)
  val transformedList: List[Int] = monadList.flatMap(simpleList)(x => List(x, x + 1))
  // applicable to Option, Try, Future, ...

  // Either is a Monad
  val manualEither: Either[String, Int] = Right(42)
  // the desirable value is the Right and error handler is the Left
  // type aliases
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._
  // we can construct Monads with Either where the Left is a concrete type and the Right is generic
  val loadingMonad: Monad[LoadingOr] = Monad[LoadingOr]
  val anEither: LoadingOr[Int] = loadingMonad.pure(45) // LoadingOr[Int] == Right(45)
  val changedLoading: LoadingOr[Int] = loadingMonad.flatMap(anEither)(n => if (n % 2 == 0) Right(n+1) else Left("Loading meaning of life..."))


  def main(args: Array[String]): Unit = {

    println(simpleList)
    println(transformedList)
  }
}
