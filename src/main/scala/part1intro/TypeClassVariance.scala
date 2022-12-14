package part1intro

object TypeClassVariance {

  import cats.Eq
  import cats.instances.int._ // brings into scope Eq[Int] type class instance (else Option~ does not compile)
  import cats.instances.option._ // constructs Eq[Option[Int]] type class instance
  import cats.syntax.eq._

  val optionComparison: Boolean = Option(2) === Option(3)
  val invalidComparison: Boolean = Some(2) === None // Eq[Some[Int]] not found.
  // Even though Some is a subtype of Option, Eq[Some[Int]] is not a subtype of Eq[Option[Int]].

  // variance
  class Animal
  class Cat extends Animal

  // covariant type: subtyping is propagated to the generic type
  class Cage[+T] // We need covariance annotation, otherwise the line below does not compile
  val cage: Cage[Animal] = new Cage[Cat] // if Cat <: Animal, then Cage[Cat] <: Cage[Animal]

  // contravariant type: subtyping is propagated backwards to the generic type (action types)
  class Vet[-T] // We need contravariance annotation, otherwise the line below does not compile
  val vet: Vet[Cat] = new Vet[Animal] // if Cat <: Animal, then Vet[Animal] <: Vet[Cat]


  // We choose object (vs class) when we need a main method (entry point)
  def main(args: Array[String]): Unit = {

  }
}
