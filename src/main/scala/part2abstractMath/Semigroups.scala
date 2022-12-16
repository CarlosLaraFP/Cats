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

  // reduces a sequence down to a single value using a function that accumulates the result, 2 elements at a time
  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)
  def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemigroup.combine)
  // type-specific APIs do not require Semigroups (i.e. the canonical list.reduce(_ + _))

  // generic (polymorphic) API
  def reduceSequence[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(semigroup.combine)
  // the power of the Semigroup is the ability to define very general combination (reduction) APIs regardless of types, given the presence of implicit Semigroups

  // the implicit type class instances that Cats provides will contain some implementations of Semigroup
  // where the combine method will do what your intuition says

  // Support a new type for a Semigroup. Create an implicit Semigroup for a new type (hint: same pattern with Eq)
  case class Expense(id: Long, amount: Double)

  // Use static factory method from Semigroup companion object
  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense] {
    // Define custom combination
    (expenseA, expenseB) => {
      val maxId = math.max(expenseA.id, expenseB.id)
      val totalAmount = expenseA.amount + expenseB.amount
      Expense(maxId, totalAmount)
    }
  }

  // extension methods from Semigroup: |+|
  import cats.syntax.semigroup._



  def main(args: Array[String]): Unit = {
    // The Semigroup is the natural combination of 2 values of the same type
    println(intCombination)
    println(stringCombination)
    val numbers = (1 to 10).toList
    println(reduceInts(numbers))
    val strings = List("a", "b", "c", "d", "e")
    println(reduceStrings(strings))

    println(reduceSequence(numbers))
    println(reduceSequence(strings))

    // brings into scope Semigroup[Option[Int]] and Semigroup[Option[String]]
    import cats.instances.option._
    val numberOptions: List[Option[Int]] = numbers.map(n => Option(n))
    // no need to unwrap options to perform the combination (sum)
    println(reduceSequence(numberOptions).getOrElse(-1))
    // this does not even compile due to Options needing unwrapping
    //println(numberOptions.reduce(_ + _))

    val testOptions: List[Option[Int]] = List(Option(1), Option(2), Option.empty[Int], Option(4))
    println(reduceSequence(testOptions).getOrElse(-1)) // 7? Yes.

    // Test custom Semigroup[Expense] to verify combination
    val expenses = List(Expense(1, 10.50), Expense(2, 11.99), Expense(3, 100.00))
    println(reduceSequence(expenses)) // Expense(3, 122.49)

    val intSum = 2 |+| 3
    println(intSum)

    println(expenses.reduce(_ |+| _))
  }
}
