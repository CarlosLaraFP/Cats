package part1recap

object TypeClasses {

  // type class is a pattern to enhance types with capabilities

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
         |"""
        .stripMargin
        .trim
  }

  // part 3: offer some API to serialize things to JSON
  def convertListToJSON[T](list: List[T])(implicit serializer: JSONSerializer[T]): String = {
    //
    list
      .map(serializer.toJson)
      .mkString("[", ",", "]")
  }

  // part 4: extending the existing types via extension methods
  object JSONSyntax {
    // implicit wrapper over a value that will take an implicit JSONSerializer as an argument
    implicit class JSONSerializable[T](value: T)(implicit serializer: JSONSerializer[T]) {
      // enhance type T with extension method
      def serialize: String = serializer.toJson(value)
    }
  }


  def main(args: Array[String]): Unit = {
    //
    val people = List(Person("Alice", 20), Person("Bob", 21))
    val serializedPeople = this.convertListToJSON[Person](people)
    println(serializedPeople)

    val person = Person("Charlie", 32)
    // This enriches the Person case class (type) to become serializable, with the corresponding extension methods, without directly extending Person
    import JSONSyntax._
    // Compiler: look for implicit class <- look for implicit JSONSerializer[Person] (subtype)
    val serializedCharlie = person.serialize
    println(serializedCharlie)
  }

}
