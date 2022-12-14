package part1recap

object TypeClasses {

  case class Person(name: String, age: Int)

  // part 1: type class definition (generic trait or abstract class)
  trait JSONSerializer[T] {
    //
    def toJson(value: T): String
  }

  // part 2: create implicit type class instances
  implicit object StringSerializer extends JSONSerializer[String] {
    // singleton because we only need a single instance of JSONSerializer[String]
    override def toJson(value: String): String = "\"" + value + "\""
  }

  implicit object IntSerializer extends JSONSerializer[Int] {
    //
    override def toJson(value: Int): String = value.toString
  }

  implicit object PersonSerializer extends JSONSerializer[Person] {
    //
    override def toJson(value: Person): String =
      s"""
         |{ "name" : ${value.name}, "age" : ${value.age} }
         |""".stripMargin
  }

  def main(args: Array[String]): Unit = {

  }

}
