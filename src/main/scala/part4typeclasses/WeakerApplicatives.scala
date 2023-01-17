package part4typeclasses

import cats.{Functor, Semigroupal}

object WeakerApplicatives {

  // Apply is a higher-kinded type class that is a weaker form of Applicative

  trait MyApply[F[_]] extends Functor[F] with Semigroupal[F] {

    override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
      val functionWrapper: F[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }
    // fundamental
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = ???
  }

  trait MyApplicative[F[_]] extends MyApply[F] {
    // fundamental
    def pure[A](x: A): F[A]
  }

  import cats.Apply
  import cats.instances.option._ // implicit Apply[Option]

  val applyOption: Apply[Option] = Apply[Option]
  val funcApp: Option[Int] = applyOption.ap(Some((x: Int) => x + 1))(Some(2)) // Some(3)

  import cats.syntax.apply._ // extension methods from Apply

  // using the product method from Apply's Semigroupal
  val tupleOptions: (Option[Int], Option[Int], Option[Int]) = (Option(1), Option(2), Option(3))
  val optionTuple: Option[(Int, Int, Int)] = tupleOptions.tupled // Some((1, 2, 3))


  def main(args: Array[String]): Unit = {
    //
    println(funcApp)
    println(optionTuple)
  }
}
