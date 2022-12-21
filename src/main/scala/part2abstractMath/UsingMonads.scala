package part2abstractMath

import scala.concurrent.Future
import scala.util.{Failure, Success}

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
  val changedLoading: LoadingOr[Int] = loadingMonad.flatMap(anEither) { n =>
    if (n % 2 == 0) Right(n + 1)
    else Left("Loading meaning of life...")
  }

  // online store to track orders
  case class OrderStatus(id: Long, status: String)

  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(OrderStatus(orderId, "Ready to ship"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] = {
    if (orderStatus.id > 1000) Left("Not available yet, refreshing data...")
    else Right("Bellevue, WA")
  }

  val orderId: Long = 457L
  val orderLocation: LoadingOr[String] = loadingMonad.flatMap(getOrderStatus(orderId))(trackLocation)

  // use extension methods
  import cats.syntax.applicative._
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  // imports not lighting up because Either belongs to the Scala standard library, which already has map/flatMap methods that are biased towards the Right

  val location: LoadingOr[String] = getOrderStatus(orderId).flatMap(trackLocation)

  val locationFor: LoadingOr[String] = for {
    orderStatus <- getOrderStatus(orderId)
    location <- trackLocation(orderStatus)
  } yield location

  // TODO: the service layer API of a web app (without changing the code below)
  // TODO - provide a real implementation of HttpService using Try, Option, Future, Either

  case class Connection(host: String, port: String)

  val config: Map[String, String] = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    // LoadingOr[Connection] in the case of Either
    def getConnection(config: Map[String, String]): M[Connection]
    // LoadingOr[String] in the case of Either
    def issueRequest(connection: Connection, payload: String): M[String]
  }

  object EitherService extends HttpService[LoadingOr] {
    // type LoadingOr[T] = Either[String, T]

    override def getConnection(config: Map[String, String]): LoadingOr[Connection] = {
      val host = config.getOrElse("host", "missing")
      val port = config.getOrElse("port", "missing")
      if (host != "missing" && port != "missing") Right(Connection(host, port))
      else Left("Unable to establish connection.")
    }

    override def issueRequest(connection: Connection, payload: String): LoadingOr[String] = {
      if (payload.length < 20) Right(s"Request $payload has been accepted.")
      else Left("Payload length must be less than 20.")
    }
  }

  // implicit Monad type class instances in scope due to Scala standard library
  val eitherRequest: LoadingOr[String] = for {
    connection <- EitherService.getConnection(config)
    response <- EitherService.issueRequest(connection, "Either HttpService")
  } yield response

  object OptionService extends HttpService[Option] {
    //
    override def getConnection(config: Map[String, String]): Option[Connection] = for {
      host <- config.get("host")
      port <- config.get("port")
    } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): Option[String] = {
      if (payload.length < 20) Some(s"Request $payload has been accepted.") else None
    }
  }

  val optionRequest: Option[String] = for {
    connection <- OptionService.getConnection(config)
    response <- OptionService.issueRequest(connection, "Option HttpService")
  } yield response

  import scala.concurrent.ExecutionContext.Implicits.global

  object FutureService extends HttpService[Future] {
    // returns subtype of Future[Connection]: Success(connection) or Failure(connection)
    override def getConnection(config: Map[String, String]): Future[Connection] = Future {
      val host = config.getOrElse("host", "missing")
      val port = config.getOrElse("port", "missing")
      if (host != "missing" && port != "missing") Connection(host, port)
      else throw new Exception("Unable to establish connection.")
    }

    override def issueRequest(connection: Connection, payload: String): Future[String] = Future {
      if (payload.length < 20) s"Request $payload has been accepted."
      else "Payload length must be less than 20."
    }
  }

  val futureRequest: Future[String] = for {
    connection <- FutureService.getConnection(config)
    response <- FutureService.issueRequest(connection, "Future HttpService")
  } yield response

  object AggressiveHttpService extends HttpService[ErrorOr] {
    // return Left(Throwable) is config not found
    override def getConnection(config: Map[String, String]): ErrorOr[Connection] = {
      if (!config.contains("host") && !config.contains("port")) Left(new RuntimeException("Unable to establish connection."))
      else Right(Connection(config("host"), config("port")))
    }

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] = {
      if (payload.length < 20) Right(s"Request $payload has been accepted.")
      else Left(new RuntimeException("Payload length must be less than 20."))
    }
  }

  val aggressiveResponse: ErrorOr[String] = for {
    connection <- AggressiveHttpService.getConnection(config)
    response <- AggressiveHttpService.issueRequest(connection, "Aggressive HttpService")
  } yield response


  def main(args: Array[String]): Unit = {
    //
    println(simpleList)
    println(transformedList)
    println(changedLoading)
    println(orderLocation)
    println(location)
    println(eitherRequest)
    println(optionRequest)
    println(futureRequest)
    println(aggressiveResponse)
  }
}
