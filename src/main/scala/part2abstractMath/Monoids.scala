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

  // How to combine case class instances using Monoid?
  final case class Dispatch(id: Int, revenue: Double)

  val seedDispatch: Dispatch = Dispatch(0, 0.0)

  implicit val dispatchMonoid: Monoid[Dispatch] = Monoid.instance[Dispatch](seedDispatch, (dispatchA, dispatchB) => {
    val mostRecentDispatch = math.max(dispatchA.id, dispatchB.id)
    val totalRevenue = dispatchA.revenue + dispatchB.revenue
    Dispatch(mostRecentDispatch, totalRevenue)
  })


  def main(args: Array[String]): Unit = {

    println(sumLeft)
    println(sumRight)
    println(numbers.foldLeft(zero)(_ |+| _))

    val dispatches = Vector(Dispatch(1, 13.99), Dispatch(2, 14.99), Dispatch(4, 15.99))
    val foldedDispatches = dispatches.foldLeft(seedDispatch)(_ |+| _)
    println(foldedDispatches)
  }
}
