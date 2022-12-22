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

  // define Monad for the identity type
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

  sealed trait Tree[+A]
  final case class Leaf[+A](value: A) extends Tree[A]
  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // define Monad for Tree (provide for-comprehension capability)
  // Monad also guarantees iteration methods are all tail recursive if tailRecM

  implicit object TreeMonad extends Monad[Tree] {
    // common sense single-value wrapper for a Tree
    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(x) => f(x)
      case Branch(x, y) => Branch(flatMap(x)(f), flatMap(y)(f))
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      //
      /*
      def headRecursion(tree: Tree[Either[A, B]]): Tree[B] = tree match {
        case Leaf(Left(x)) => headRecursion(f(x))
        case Leaf(Right(x)) => Leaf(x)
        case Branch(left, right) => Branch(headRecursion(left), headRecursion(right))
      }
      headRecursion(f(a))
      */

      @scala.annotation.tailrec
      def tailRec(todo: List[Tree[Either[A, B]]], expanded: List[Tree[Either[A, B]]], done: List[Tree[B]]): Tree[B] = {
        if (todo.isEmpty) done.head
        else todo.head match {
          case Leaf(Left(x)) => tailRec(f(x) :: todo.tail, expanded, done)
          case Leaf(Right(x)) => tailRec(todo.tail, expanded, Leaf(x) :: done)
          case node @ Branch(left, right) =>
            if (!expanded.contains(node)) tailRec(right :: left :: todo, node :: expanded, done)
            else tailRec(todo.tail, expanded, Branch(done.head, done.tail.head) :: done.drop(2))
        }
      }
      tailRec(List(f(a)), Nil, Nil)
    }
  }


  def main(args: Array[String]): Unit = {
    //
    println(IdentityMonad.flatMap(aNumber)(_.toString))
    println(IdentityMonad.flatMap(aNumber)(_ + 1))
    println(TreeMonad.flatMap(Leaf(2))(v => Leaf(v + 1)))
    println(TreeMonad.flatMap(Branch(Leaf(10), Leaf(20)))(v => Leaf(v * 10)))
    println(TreeMonad.flatMap(Branch(Leaf(10), Leaf(20)))(v => Branch(Leaf(v + 1), Leaf(v + 2))))
    println(TreeMonad.iterateUntilM(1)(x => Leaf(x + 1))(_ == 10))
  }
}
