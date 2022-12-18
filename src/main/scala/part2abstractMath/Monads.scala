package part2abstractMath

object Monads {

  // lists
  val numbersList: List[Int] = List(1, 2, 3)
  val charsList: List[Char] = List('a', 'b', 'c')
  
  // TODO: Create all combinations of (number, char)
  val combinationIntChar: List[(Int, Char)] = for {
    i <- numbersList
    j <- charsList
  } yield (i, j)
  

  def main(args: Array[String]): Unit = {

    println(combinationIntChar)

  }
}
