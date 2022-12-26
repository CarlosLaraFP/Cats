package part3datamanipulation

import cats.Id
import cats.data.WriterT

import scala.annotation.tailrec

object Writers {

  // Data type to let you keep track of useful information while data is being manipulated
  import cats.data.Writer
  // Use cases: Metadata, lineage (i.e. in Hudi no more string vars throughout)
  // The type parameters can be custom user-defined types where we supply the Semigroup/Monoid

  // logs type and value type parameters (basically Option[T] with additional information to be transformed)
  val writer: Writer[List[String], Int] = Writer(List("Started process"), 45)

  val increasedWriter: Writer[List[String], Int] = writer.map(_ + 1) // value increases, logs stay the same
  val logsWriter: Writer[List[String], Int] = increasedWriter.mapWritten(_ :+ "Part 1 completed") // value stays the same, logs change
  val bothWriter: Writer[List[String], Int] = logsWriter.bimap(_ :+ "Part 2 completed", _ + 1) // both
  // this is useful when including value in the logs
  val bothWriterTwo: Writer[List[String], Int] = bothWriter.mapBoth { (logs, value) =>
    (logs :+ s"Part 3 completed with value $value", value + 1)
  }

  /*
    1. Define Writer(s) at the start
    2. Manipulate them with pure FP
    3. Dump either the value or the logs
  */

  val desiredValue: Int = bothWriterTwo.value
  val logs: List[String] = bothWriterTwo.written
  val (l, v) = bothWriterTwo.run

  val writerA: Writer[Vector[String], Int] = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB: Writer[Vector[String], Int] = Writer(Vector("Log B1"), 40)

  // Writers compose just like Options (flatMap)
  // The logs are combined per their natural combine method (in the presence of Semigroup)
  import cats.instances.vector._ // imports implicit Semigroup[Vector] and Monoid[Vector]

  val compositeWriter: Writer[Vector[String], Int] = for {
    a <- writerA
    b <- writerB
  } yield a + b

  import cats.instances.list._ // imports implicit Monoid[List[Int]] to supply the "starting" value upon writer.reset
  // reset logs
  val emptyWriter: Writer[List[String], Int] = bothWriterTwo.reset

  // TODO: Re-write using Writers
  //@tailrec
  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("Starting...")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    @tailrec
    def writerHelper(writer: Writer[Vector[String], Int]): Writer[Vector[String], Int] = {
      if (writer.value == n) writer.mapWritten(_ appended n.toString)
      else writerHelper(writer.mapBoth { (logs, value) =>
        (logs appended value.toString, value + 1)
      })
    }
    writerHelper(Writer(Vector.empty[String], 1))
  }

  // TODO: Re-write using Writers
  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}) = $lowerSum")
      lowerSum + n
    }
  }

  def writerSum(n: Int): Writer[Vector[Int], Int] = {
    @tailrec
    def sumHelper(i: Int, writer: Writer[Vector[Int], Int]): Writer[Vector[Int], Int] = {
      if (i == 0) writer
      else sumHelper(i - 1, writer.mapBoth { (logs, value) =>
        (logs appended value, value + i - 1)
      })
    }
    sumHelper(n, Writer(Vector.empty[Int], n))
  }


  def main(args: Array[String]): Unit = {
    //
    println(bothWriterTwo)
    println(desiredValue)
    println(logs)
    println(v)
    println(l)
    println(compositeWriter.run) // returns tuple
    println(emptyWriter.run)
    //countAndSay(10)
    val recursiveWriter = countAndLog(10)
    println(recursiveWriter.run)
    //println(naiveSum(3))
    println(writerSum(3))
  }
}
