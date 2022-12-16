package part2abstractMath

object Monoids {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._ // import extension method |+|

  val numbers: List[Int] = (1 to 1000).toList
  // |+| is always associative (it always produces the same result regardless of order)
  val sumLeft: Int = numbers.foldLeft(0)(_ |+| _)
  val sumRight: Int = numbers.foldRight(0)(_ |+| _)

  // define a general API
  //def combineFold[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.tail.foldLeft(list.head)(_ |+| _)

  // extending Semigroup idea to another type class: trait Monoid[T] ... extends Semigroup[T]
  // Semigroup (combine) => Monoid (combine and empty)
  import cats.Monoid
  // Monoid is like a Semigroup, but with the added capability to provide seed values for folding
  val intMonoid: Monoid[Int] = Monoid[Int]
  val combineInt: Int = intMonoid.combine(23, 999)
  val zero: Int = intMonoid.empty // natural seed/zero value for the type (i.e. 0 for Int and "" for String)

  import cats.instances.string._
  val emptyString: String = Monoid[String].empty
  val combineString: String = Monoid[String].combine("I love ", "Scala")


  def main(args: Array[String]): Unit = {

    println(sumLeft)
    println(sumRight)

  }
}
