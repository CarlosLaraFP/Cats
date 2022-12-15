package part1intro

object TypeClassVariance {

  import cats.Eq
  import cats.instances.int._ // brings into scope Eq[Int] type class instance (else Option~ does not compile)
  import cats.instances.option._ // constructs Eq[Option[Int]] type class instance
  import cats.syntax.eq._

  val optionComparison: Boolean = Option(2) === Option(3)
  //val invalidComparison: Boolean = Some(2) === None // Eq[Some[Int]] not found.
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

  // rule of thumb
  // Covariance: if a generic type "HAS a T"
  // Contravariance: if a generic type "ACTS on T"
  // variance affects how type class instances are fetched

  trait SoundMaker[-T] // contravariant type class
  implicit object AnimalSoundMaker extends SoundMaker[Animal] // contravariant type class instance
  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("Evolution")
  // RULE: contravariant type classes can use the superclass instances if nothing is available strictly for that type
  implicit object OptionSoundMaker extends SoundMaker[Option[Int]] // contravariant type class instance

  // covariant type class
  trait AnimalShow[+T] {
    def show: String
  }
  // covariant type class instance
  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "Animals"
  }
  // covariant type class instance
  implicit object CatsShow extends AnimalShow[Cat] {
    override def show: String = "Cats"
  }
  // define API for users to call
  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show
  // RULE: covariant type classes will always  use the more specific type class instance for that type (confuses the compiler if the general TC is also present)

  // RULE: we cannot have both => this is why Cats uses invariant type classes
  val validComparison: Boolean = Option(2) === Option.empty[Int]

  // TAKEAWAY: Always use the general type with smart constructors


  // We choose object (vs class) when we need a main method (entry point)
  def main(args: Array[String]): Unit = {
    //
    makeSound[Animal]
    makeSound[Cat] // type class instance for Animal is also applicable to Cat (requires contravariant annotation to compile)
    // SoundMaker[Cat] is not found, but the compiler finds a "better" instance (SoundMaker[Animal])

    makeSound[Option[Int]]
    makeSound[Some[Int]] // if it works on the supertype, it works on the subtype

    println(organizeShow[Cat])
    // println(organizeShow[Animal]) // does not compile due to ambiguous type class instances
  }
}
