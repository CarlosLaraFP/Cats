package part3datamanipulation

import cats.Id
import cats.data.WriterT

object Writers {

  // Data type to let you keep track of useful information while data is being manipulated
  import cats.data.Writer

  // logs type and value type parameters (basically Option[T] with additional information to be transformed)
  val writer: Writer[List[String], Int] = Writer(List("Started process"), 45)

  val increasedWriter: WriterT[Id, List[String], Int] = writer.map(_ + 1) // value increases, logs stay the same
  val logsWriter: WriterT[Id, List[String], Int] = writer.mapWritten(_ :+ "Part 1 completed") // value stays the same, logs change
  val bothWriter: WriterT[Id, List[String], Int] = writer.bimap(_ :+ "Part 2 completed", _ + 1) // both
  // this is useful when including value in the logs
  val bothWriterTwo: WriterT[Id, List[String], Int] = writer.mapBoth { (logs, value) =>
    (logs :+ s"Part 3 completed with value $value", value + 1)
  }


  def main(args: Array[String]): Unit = {
    //
    println(bothWriterTwo)
  }
}
