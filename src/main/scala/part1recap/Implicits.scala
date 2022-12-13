package part1recap

object Implicits {

  // extension method pattern with implicit classes (one-argument wrappers over values to help with implicit conversions)

  case class Person(name: String) {
    def greet: String = s"Hi, my name is $name!"
  }

  // The compiler requires the single constructor argument (implicit class must have a primary constructor with exactly one argument in first parameter list)
  implicit class ImpersonableString(name: String) {
    def greet: String = Person(name).greet
  }
  // Essentially the Scala way of defining extension methods? Yes!
  // An implicit class automatically wraps a value into another kind of value and it offers extension methods to enrich the existing type.

  // The greet method does not belong to the String class (only possible with the implicit class defined)
  // The compiler tries to find something that can wrap a String that can have a greet method
  val greeting: String = "Liam".greet // Compiler: new ImpersonableString("Liam").greet

  // importing implicit conversions in scope
  import scala.concurrent.duration._
  val oneSecond: FiniteDuration = 1.second // implicit class (conversion) from Int to FiniteDuration

  // implicit arguments and values
  def increment(x: Int)(implicit amount: Int): Int = x + amount
  implicit val defaultAmount: Int = 10
  val incremented2: Int = increment(2) // Compiler: implicit argument 10 (in scope) is passed by the compiler

  def multiply(x: Int)(implicit times: Int): Int = x * times
  val times2: Int = multiply(2)

  // a generic trait that converts any type to a String (this is called JSON serialization)
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // specific type conversions must be defined as concrete implementations of the abstract method
  def listToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String = {
    // returns the canonical representation of an array in JSON
    list.map(serializer.toJson).mkString("[", ",", "]")
  }

  val persons: List[Person] = List(Person("Alice"), Person("Bob"))

  implicit val personSerializer: JSONSerializer[Person] = new JSONSerializer[Person] {
    // use the triple quote so that we can also write the single quote character inside without needing to escape every single thing
    override def toJson(person: Person): String =
      s"""
         |{"name" : "${person.name}"
         |""".stripMargin
  }

  val personsJson: String = listToJson[Person](persons)

  def main(args: Array[String]): Unit = {

  }

}
