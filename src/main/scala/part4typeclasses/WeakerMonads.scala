package part4typeclasses

import cats.{Applicative, Apply}


object WeakerMonads {

  trait MyFlatMap[M[_]] extends Apply[M] {
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    // Per Daniel, "if you nailed this function, then massive massive congratulations"
    override def ap[A, B](ff: M[A => B])(fa: M[A]): M[B] = flatMap(ff)(f => map(fa)(f))
    // Does ap work both ways? Yes because they commute, as long as there are no side-effects that require sequential execution (i.e. Effect]
  }

  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M] {
    override def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))
  }

  import cats.FlatMap
  import cats.syntax.flatMap._ // flatMap extension method
  import cats.syntax.functor._ // map extension method

  def getPairs[M[_] : FlatMap, A, B](elementsA: M[A], elementsB: M[B]): M[(A, B)] = for {
    a <- elementsA
    b <- elementsB
  } yield (a, b)


  def main(args: Array[String]): Unit = {
    //

  }
}
