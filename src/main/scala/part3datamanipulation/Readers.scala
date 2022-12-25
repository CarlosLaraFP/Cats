package part3datamanipulation

//import cats.Id
//import cats.instances.long._
import cats.data.EitherT
import cats.instances.future._

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}


object Readers {

  /*
    - fetch configuration file => feeds initial data structure(s)
    informs:
    - a database layer
    - an HTTP layer
    - a business logic layer
  */
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  case class Configuration(dbUsername: String, dbPassword: String, host: String, port: Int, nThreads: Int, emailReplyTo: String)

  type AsyncResponse[T] = EitherT[Future, String, T]

  case class DbConnection(username: String, password: String) {
    // In practice: SELECT * FROM Table and return the status of the orderID (Query in reactive architecture)
    def getOrderStatus(orderId: Long): String = s"Palantir $orderId"
    // In practice: SELECT MAX(orderId) FROM Table WHERE username = username
    def getLastOrderId(username: String): Long = 5237623
      //if (username.nonEmpty) EitherT.right(5237623) else EitherT.left("Invalid username.")
  }

  case class HttpService(host: String, port: Int) {
    // this would start the actual server
    def start(): Unit = println("Server started")
  }

  // bootstrap phase when we spin up the application
  // create an initial data structure from which everything else can be derived
  val config: Configuration = Configuration("Carlos", "Valinor", "localhost", 1234, 8, "clara@valinor.ai")

  // Reader is a data processing type that will implement the pattern: Configuration => Initialize System
  import cats.data.Reader

  // Creating type alias for re-usability
  type MicroserviceReader[T] = Reader[Configuration, T]

  // type parameters: input and output (wrapper over a function that maps I to O)
  // this Reader is the specification of how we are going to derive a DbConnection from a Configuration object
  val dbReader: MicroserviceReader[DbConnection] = Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword))
  val dbConnection: DbConnection = dbReader.run(config)

  // Reader[I, O]
  val orderStatusReader: MicroserviceReader[String] = dbReader.map(dbConn => dbConn.getOrderStatus(55))
  val orderStatus: String = orderStatusReader.run(config) // feels lazy

  def getLastOrderStatus(username: String): String =
    dbReader
      .map(_.getLastOrderId(username))
      .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))
      .run(config)

  def getLastOrderStatusFor(username: String): String =  {
    // equivalent to above
    val order: MicroserviceReader[String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      lastOrderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield lastOrderStatus // yields this wrapped in the type

    order.run(config)
  }

  /*
    Pattern
    1. Create initial data structure
    2. Create a Reader that specifies how the data structure will be manipulated later
    3. map & flatMap the Reader to produce derived information
    4. When we need the final piece of information, call run on the Reader with the initial data structure
  */

  case class EmailService(emailReplyTo: String) {
    //
    def sendEmail(address: String, contents: String) = s"From $emailReplyTo to $address >>> $contents"
  }

  val emailReader: MicroserviceReader[EmailService] = Reader(conf => EmailService(conf.emailReplyTo))

  def emailUser(username: String, userEmail: String): String = {
    // fetch the status of the last order and email with the email service
    val emailService: MicroserviceReader[String] = for {
      orderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(orderId))
      email <- emailReader.map(_.sendEmail(userEmail, s"$username's last order: $orderStatus"))
    } yield email

    emailService.run(config)
  }

  // What programming pattern do readers remind you of? Functional dependency injection.


  def main(args: Array[String]): Unit = {
    //
    //println(dbConnection)
    //println(orderStatus)
    println(getLastOrderStatus("Carlos"))
    println(getLastOrderStatusFor("Carlos"))
    println(emailUser("Elendil", "elendil@numenor.ai"))
  }
}
