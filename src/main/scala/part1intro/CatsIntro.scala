package part1intro

object CatsIntro {

  // Eq is a type class that allows you to compare values at compile type
  //val comparison = 2 == "string"
  // Eq was invented to have an extension method on any type to automatically check whether types are the same
  // Using cats.Eq, the code above does not even compile

  // part 1: type class import
  import cats.Eq

  // part 2: import type class instances for the types you need (singleton objects, like TypeClasses.scala)
  import cats.instances.int._

  // part 3: explicitly use the type class API
  val intEquality: Eq[Int] = Eq[Int]
  val typeSafeComparison: Boolean = intEquality.eqv(2, 3) // false

  // part 4: or use extension methods (if applicable)
  import cats.syntax.eq._
  // brings into scope all extension methods that type class eq supports
  val anotherTypeSafeComparison: Boolean = 2 === 3
  val notEqualComparison: Boolean = 2 =!= 3
  // extension methods are only visible in the presence of the right type class instance

  // part 5: extending type class operations to composite types (i.e. List)
  import cats.instances.list._ // we bring Eq[List[Int]] in scope
  val listComparison: Boolean = List(2) === List(3)

  // part 6: what if our types are not supported by Cats? Create a type class instance for a custom type
  case class Car(model: String, price: Double)
  // Two cars === true if they have the same price

  //implicit object CarEq extends Eq[Car] {
  //  override def eqv(x: Car, y: Car): Boolean = x.price == y.price
  //}
  // Best practice (cleaner and more maintainable) using static factory method from Eq companion object
  implicit val carEq: Eq[Car] = Eq.instance[Car] {
    (car1, car2) => car1.price == car2.price
  }


  def main(args: Array[String]): Unit = {
    //
    println(typeSafeComparison)
    println(anotherTypeSafeComparison)
    println(notEqualComparison)
    println(listComparison)
    println(Car("Maserati", 100000.50) === Car("Aston Martin", 101000.90))
    println(Car("Bentley", 300000.50) === Car("Rolls Royce", 300000.50))
  }

}
