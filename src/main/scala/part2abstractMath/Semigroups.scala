package part2abstractMath

// Semigroups combine elements of the same type
object Semigroups {

  //
  import cats.Semigroup
  import cats.instances.int._

  // fetches implicit type class instance
  val naturalIntSemigroup: Semigroup[Int] = Semigroup[Int]
  val intCombination: Int = naturalIntSemigroup.combine(2, 46) // addition

  import cats.instances.string._
  val naturalStringSemigroup: Semigroup[String] = Semigroup[String]
  val stringCombination: String = naturalStringSemigroup.combine("I love ", "Cats")

  // the implicit type class instances that Cats provides will contain some implementations of Semigroup
  // where the combine method will do what your intuition says

  def main(args: Array[String]): Unit = {
    println(intCombination)
    println(stringCombination)
  }
}
