package part2abstractMath

object MonadTransformers {

  def sumAllOptions(values: List[Option[Int]]): Int = values.foldLeft(0)((runningTotal, x) => x match {
    case None => runningTotal
    case Some(v) => runningTotal + v
  })

  // allows combining Options without unwrapping/wrapping all over the place
  import cats.data.OptionT
  import cats.instances.list._ // fetch an implicit OptionT[Monad[List]]

  // List[Option[Int]]
  val listIntOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))
  // OptionT has map and flatMap methods
  val listTuples: OptionT[List, (Int, Char)] = for {
    c <- listCharOptions
    i <- listIntOptions
  } yield (i, c)

  import cats.data.EitherT



  def main(args: Array[String]): Unit = {
    //
    println(sumAllOptions(List(Option(10), Option.empty[Int], Option(20))))
    println(listTuples.value)
  }
}
