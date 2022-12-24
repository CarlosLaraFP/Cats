package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers {

  def sumAllOptions(values: List[Option[Int]]): Int = values.foldLeft(0)((runningTotal, x) => x match {
    case None => runningTotal
    case Some(v) => runningTotal + v
  })

  // allows combining Options without unwrapping/wrapping all over the place
  import cats.data.OptionT
  import cats.instances.list._
  import cats.data.EitherT
  import cats.instances.future._
  // Wow, somehow the implicit global ExecutionContext does not work
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  /*
    val intsRegular: List[Option[Int]]= List(Option(1), Option(2))
    val charsRegular: List[Option[Char]]= List(Option('a'), Option('b'), Option.empty[Char])

    val tuplesRegular: List[(Int, Char)] = for {
      c <- charsRegular
      i <- intsRegular
    } yield (i.get, c.get) // throws runtime exception in the case of None
  */

  // List[Option[Int]]
  val listIntOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))
  // OptionT has map and flatMap methods
  // Using OptionT (monad transformer for Option) allows for-comprehensions to access inner values
  // without needing to wrap/unwrap (due to flatMap/map) and preventing runtime exceptions
  val listTuples: OptionT[List, (Int, Char)] = for {
    c <- listCharOptions
    i <- listIntOptions
  } yield (i, c)

  val vectorEither: EitherT[Vector, String, Int] = EitherT(Vector(Left("error"), Right(43), Right(11)))
  val futureEither: EitherT[Future, String, Int] = EitherT.right(Future(43)) // wrap over Future(Right(45))

  val bandwidths: Map[String, Int] = Map(
    "server1.valinor.com" -> 50,
    "server2.valinor.com" -> 300,
    "server3.valinor.com" -> 170
  )

  // Future[Either[String, T]]
  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None => EitherT.left(Future(s"Server $server unreachable.")) // wrap over Future(Left("..."))
    case Some(x) => EitherT.right(Future(x)) // // wrap over Future(Right(some int))
  }

  // Future[Either[String, Boolean]]
  // if either getBandwidth returns Left of String, the method returns it instead of yielding Right of Boolean
  def canWithstandSurge(server1: String, server2: String): AsyncResponse[Boolean] = for {
    response1 <- getBandwidth(server1)
    response2 <- getBandwidth(server2)
  } yield response1 + response2 > 250

  // transforms the Either[String, Future] inside into Either[String, String] and returns it wrapped in a Future
  def generateTrafficSpikeReport(server1: String, server2: String): AsyncResponse[String] =
    canWithstandSurge(server1, server2).transform {
      case Left(reason) => Left(s"Servers $server1 and $server2 cannot absorb the incoming spike: $reason.")
      case Right(false) => Left(s"Servers $server1 and $server2 cannot absorb the incoming spike: Not enough total bandwidth.")
      case Right(true) => Right(s"Servers $server1 and $server2 can absorb the incoming spike.")
    }


  def main(args: Array[String]): Unit = {
    //
    println(sumAllOptions(List(Option(10), Option.empty[Int], Option(20))))
    println(listTuples.value)
    //println(tuplesRegular)

    val resultFuture = generateTrafficSpikeReport("server1.valinor.com", "server2.valinor.com").value
    resultFuture foreach println
  }
}
