package part2abstractMath

object Monoids {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.monoid._ // import extension method |+|

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

  import cats.instances.option._
  val emptyOption: Option[Int] = Monoid[Option[Int]].empty // None, per intuition
  val combinedOptions: Option[Int] = Monoid[Option[Int]].combine(Option(3), Option.empty[Int])

  // a benefit of this approach is abstracting the seed value parameter
  def monoidFold[T](seq: Seq[T])(implicit monoid: Monoid[T]): T = seq.foldLeft(monoid.empty)(_ |+| _)

  // I'd think this is too universal that Cats would provide it
  //val emptyMap: Map[String, Int] = Map.empty[String, Int]
  //implicit val mapMonoid: Monoid[Map[String, Int]] = Monoid.instance[Map[String, Int]](emptyMap, _ ++ _)
  import cats.instances.map._

  // shopping cart and online stores with Monoids
  case class ShoppingCart(items: List[String], total: Double)

  val emptyShoppingCart: ShoppingCart = ShoppingCart(List.empty[String], 0.0)

  implicit val shoppingMonoid: Monoid[ShoppingCart] = Monoid.instance[ShoppingCart](emptyShoppingCart, (cartA, cartB) => {
    ShoppingCart(cartA.items ++ cartB.items, cartA.total + cartB.total)
  })

  def checkout(shoppingCarts: Seq[ShoppingCart]): ShoppingCart = monoidFold(shoppingCarts)

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
    // Monoid[Int]
    println(numbers.foldLeft(zero)(_ |+| _))
    println(monoidFold(numbers))
    // Monoid[Option[Int]]
    val nullableInts = numbers.map(n => Option(n))
    println(nullableInts.foldLeft(emptyOption)(_ |+| _))
    println(monoidFold(nullableInts))
    // Monoid[Map[String, Int]]
    val phoneBooks: List[Map[String, Int]] = List(
      Map("Alice" -> 235, "Bob" -> 647),
      Map("Charlie" -> 372, "Daniel" -> 889),
      Map("Rita" -> 123)
    )
    // essentially an inner flatMap
    println(monoidFold(phoneBooks))
    //println(phoneBooks.flatMap()) // ???
    val shoppingCarts: Vector[ShoppingCart] = Vector(
      ShoppingCart(List("bookB", "shirt"), 16.22),
      ShoppingCart(List("bookA", "beanie"), 19.11),
      ShoppingCart(List("grass", "treats"), 42.99),
      ShoppingCart(List(), 0.0)
    )
    println(checkout(shoppingCarts))
    println(monoidFold(shoppingCarts))

    // Monoid[Dispatch]
    val dispatches = List(Dispatch(4, 13.99), Dispatch(2, 14.99), Dispatch(1, 15.99))
    val foldedDispatches = dispatches.foldLeft(seedDispatch)(_ |+| _)
    println(foldedDispatches)
    println(monoidFold(dispatches))
    // Monoid[Option[Dispatch]]
    val nullableDispatches = dispatches.map(d => Option(d))
    println(monoidFold(nullableDispatches))
  }
}
