package part3datamanipulation

import cats.Id

object Readers {

  /*
    - fetch configuration file => feeds initial data structure(s)
    informs:
    - a database layer
    - an HTTP layer
    - a business logic layer
  */

  case class Configuration(dbUsername: String, dbPassword: String, host: String, port: Int, nThreads: Int)

  case class DbConnection(username: String, password: String) {
    // In practice: SELECT * FROM Table and return the status of the orderID (Query in reactive architecture)
    def getOrderStatus(orderId: Long): String = s"Dispatched $orderId"
    // In practice: SELECT MAX(orderId) FROM Table WHERE username = username
    def getLastOrderId(username: String): Long = 5237623
  }

  case class HttpService(host: String, port: Int) {
    // this would start the actual server
    def start(): Unit = println("Server started")
  }

  // bootstrap phase when we spin up the application
  // create an initial data structure from which everything else can be derived
  val config: Configuration = Configuration("Carlos", "Valinor", "localhost", 1234, 8)

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

  def getLastOrderStatus(username: String): String = dbReader
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




  def main(args: Array[String]): Unit = {
    //
    println(dbConnection)
    println(orderStatus)
    println(getLastOrderStatus("Carlos"))
    println(getLastOrderStatusFor("Carlos"))
  }
}
