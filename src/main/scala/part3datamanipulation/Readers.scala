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
    // SELECT * FROM Table and return the status of the orderID (Query in reactive architecture)
    def getOrderStatus(orderId: Long): String = "Dispatched"
  }

  case class HttpService(host: String, port: Int) {
    // this would start the actual server
    def start(): Unit = println("Server started")
  }

  // bootstrap phase when we spin up the application
  val config: Configuration = Configuration("Carlos", "Valinor", "localhost", 1234, 8)

  // Reader is a data processing type that will implement the pattern: Configuration => Initialize System
  import cats.data.Reader
  // type parameters: input and output (wrapper over a function that maps input to output)
  // this Reader is the specification of how we are going to derive a DbConnection from a Configuration object
  val dbReader: Reader[Configuration, DbConnection] = Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword))
  val dbConnection: Id[DbConnection] = dbReader.run(config)


  def main(args: Array[String]): Unit = {
    //
    println(dbConnection)
  }
}
