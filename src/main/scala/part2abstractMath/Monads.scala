package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object Monads {

  // Lists
  val numbersList: List[Int] = List(1, 2, 3)
  val charsList: List[Char] = List('a', 'b', 'c')
  
  // Create all combinations of (number, char)
  val combinationIntChar: List[(Int, Char)] = for {
    i <- numbersList
    c <- charsList
  } yield (i, c)
  // the compiler collapses for comprehensions into chains of flatMap and map

  // flatMap works because the inner map yields 3 Lists, one for each i
  val combinationsList: List[(Int, Char)] = numbersList.flatMap(i => charsList.map(c => (i, c)))

  // Options
  val numberOption: Option[Int] = Option(2)
  val charOption: Option[Char] = Option('c')
  // create the combination
  val combinationOption: Option[(Int, Char)] = numberOption.flatMap(i => charOption.map(c => (i, c)))
  val combinationOptionFor: Option[(Int, Char)] = for {
    i <- numberOption
    c <- charOption
  } yield (i, c)

  // Futures
  // platform on top of which Futures will be running
  // Returns a default thread factory used to create new threads. This factory creates all new threads used by an Executor in the same ThreadGroup... Each new thread is created as a non-daemon thread
  //implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  // Because these are non-daemon threads the program does not terminate.
  // On the other hand, for example, scala.concurrent.ExecutionContext.Implicits.global creates daemon threads
  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global
  // Daemon thread in JVM is a low-priority thread that runs in the background to perform tasks such as garbage collection. Daemon thread in Java is also a service provider thread that provides services to the user thread.
  /*
  The documentation for scala.concurrent.ExecutionContext.Implicits.global on the ExecutionContext trait reads:

  It is possible to simply import scala.concurrent.ExecutionContext.Implicits.global to obtain an implicit ExecutionContext.
  This global context is a reasonable default thread pool.

  Default

  It's a fixed size ThreadPool, which has as many threads as the processors on the machine. A reasonable default means it's good for most things most of the time.

  What a "good" thread pool is

  First, it's important to understand you only have as many threads as the cores on the machine. All other threads are so called demon threads and it's all about being smart with queue-ing and executing.(at language/library level).

  CachedThreadPool: Many short lived/cheap tasks

  The type of thread pools you spawn vastly depend on the actions they are mean to perform. For a lot of short lived actions(say database queries), you would go a with a cached thread pool.

  Because each individual task is relatively cheap but spawning a new thread is expensive, you are better off with a CachedThreadPool.

  FixedThreadPool: Long running/expensive tasks

  In contrast with the above, for very expensive operations, you probably want to limit the amount of threads running at one time, for various reasons: memory, performance etc.

  ForkJoinPool: Divide et impera

  This type of pool is useful when you need to perform a very large computation, but you can divide it into smaller bits individual workers can compute.

  The list goes on and on. Bottom line, Scala gives you something in between all of the above. Specifically, Scala tries to create a ForkJoinPool and defaults to a ThreadPoolExecutor if the first fails.
  */


  // will be evaluated at some point in the future on some thread of the ExecutionContext
  val numberFuture: Future[Int] = Future(42)
  val charFuture: Future[Char] = Future('Z')
  // create the combination
  val combinationFutures: Future[(Int, Char)] = numberFuture.flatMap(i => charFuture.map(c => (i, c)))
  val combinationFuturesFor: Future[(Int, Char)] = for {
    i <- numberFuture
    c <- charFuture
  } yield (i, c)

  /*
    Pattern
    1. Ability to wrap a regular value into a wrapper / container (monadic value)
    2. Ability to transform these M values into another M value via flatMap

    Note: flatMap applies to general data structures beyond sequential types (however, flatMap does guarantee a sequential order of operation)

    Monad is the higher kinded type class that formalizes this pattern.
  */
  trait MyMonad[M[_]] {
    // if M is a List and A is an Int, pure (wrap) transforms Int into List[Int]
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    // Monad extends Functor because it's able to provide its fundamental method (map)
    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))
  }

  import cats.Monad
  import cats.instances.option._ // implicit Monad[Option]
  val optionMonad: Monad[Option] = Monad[Option]
  val anOption: Option[Int] = optionMonad.pure(4) // Option(4) == Some(4)
  val transformedOption: Option[Int] = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)

  import cats.instances.list._
  val listMonad: Monad[List] = Monad[List]
  val list: List[Int] = listMonad.pure(3)
  val transformedList: List[Int] = listMonad.flatMap(list)(x => List(x, x + 1))

  import cats.instances.future._
  val futureMonad: Monad[Future] = Monad[Future] // requires an implicit ExecutionContext in scope
  val future: Future[Boolean] = futureMonad.pure(true)
  val transformedFuture: Future[Int] = futureMonad.flatMap(future)(x => if (x) Future(1) else Future(0))

  // Monad is useful for general APIs (otherwise repeated code like below)
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))
  def getPairsOption(number: Option[Int], char: Option[Char]): Option[(Int, Char)] = number.flatMap(n => char.map(c => (n, c)))
  def getPairsFuture(number: Future[Int], char: Future[Char]): Future[(Int, Char)] = number.flatMap(n => char.map(c => (n, c)))

  // generalize
  def getPairsX[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] = monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  // extension methods (weirder imports - pure, flatMap)
  import cats.syntax.applicative._ // the pure extension method is here (Applicative is a subtype of Monad)
  val oneOption: Option[Int] = 1.pure[Option] // implicit Monad[Option] will be used => Some(1)
  val oneList: List[Int] = 1.pure[List] // List(1)

  import cats.syntax.flatMap._ // the flatMap extension method is here
  val oneOptionTransformed: Option[Int] = oneOption.flatMap(x => (x + 1).pure[Option])

  import cats.syntax.functor._

  def getPairs[M[_] : Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] = ma.flatMap(a => mb.map(b => (a, b)))


  def main(args: Array[String]): Unit = {

    println(combinationIntChar)
    println(combinationsList)
    println(combinationOption)
    println(combinationFutures)
    println(combinationFuturesFor)
    println(transformedOption)
    println(transformedList)
    println(transformedFuture)
    println(getPairs(numbersList, charsList))
    println(getPairs(numberOption, charOption))
    getPairs(numberFuture, charFuture) foreach println
  }
}
