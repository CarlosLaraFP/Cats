package part2abstractMath

object CustomMonads {

  import cats.Monad

  // The Monad type has additional iteration methods because Monad == general sequential computations
  // We can represent iteration (imperative programming) as tail recursion using immutable data structures (functional programming)

  implicit object OptionMonad extends Monad[Option] {
    //
    override def pure[A](x: A): Option[A] = Option(x)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    // all other iteration methods are based on this implementation
    @scala.annotation.tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case None => None
      case Some(Left(v)) => tailRecM(v)(f)
      case Some(Right(b)) => Some(b)
    }
  }

  // TODO: define Monad for the identity type
  type Identity[T] = T
  val aNumber: Identity[Int] = 42

  // generic extension methods
  import cats.syntax.applicative._
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  implicit object IdentityMonad extends Monad[Identity] {
    //
    override def pure[A](x: A): Identity[A] = x

    // blindly using generic flatMap does not work (domain context matters)
    override def flatMap[A, B](a: Identity[A])(f: A => Identity[B]): Identity[B] = f(a)

    @scala.annotation.tailrec
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Right(x) => x
      case Left(x) => tailRecM(x)(f)
    }
  }


  def main(args: Array[String]): Unit = {
    //
    println(IdentityMonad.flatMap[Int, String](aNumber)(_.toString))
    println(IdentityMonad.flatMap[Int, Int](aNumber)(_ + 1))
  }
}
