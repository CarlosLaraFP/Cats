package part3datamanipulation

import cats.data.Validated.Valid

import scala.util.Try


object DataValidation {

  /*
    Validated acts like an Either, where Left is undesired and Right is desired
    Validated has a different contract than Either.
    It's used to combine all errors using 1 value without mutations (pure FP).
  */
  import cats.data.Validated

  val validValue: Validated[String, Int] = Validated.valid(42) // "Right" in Either
  val invalidValue: Validated[String, Int] = Validated.invalid("Something went wrong.") // "Left" in Either

  val test: Validated[String, Int] = Validated.cond(42 > 39, 99, "Meaning of life is too small.")

  def testNumber(n: Int): Either[List[String], Int] = {
    /*
      n must be prime (whole number > 1 that cannot be exactly divided by any whole number other than itself and 1 (e.g. 2, 3, 5, 7, 11)
      n must be non-negative
      n must be <= to 100
      n must be even

      Left(List[String]) contains every condition that n fails. Else return Right(n)
    */
    val errors = scala.collection.mutable.ListBuffer[String]()

    List(
      ("n must be prime", (2 until n-1) exists (n % _ == 0)),
      ("n must be non-negative", n < 0),
      ("n must be <= to 100", n > 100),
      ("n must be even", n % 2 != 0)
    ).foreach(element => if (element._2) errors += element._1)

    if (errors.nonEmpty) Left(errors.toList) else Right(n)
  }

  //import cats.instances.list._
  import cats.Semigroup

  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance[Int](scala.math.max)
  implicit val combineLinkedList: Semigroup[List[String]] = Semigroup.instance[List[String]](_ ::: _)

  def validateNumber(n: Int): Validated[List[String], Int] = {
    // combine belongs to Semigroup (we need combine for both List[String] and Int)
    Validated
      .cond(n % 2 == 0, n, List("n must be even"))
      .combine(Validated.cond(n > 0, n, List("n must be non-negative")))
      .combine(Validated.cond(n < 100, n, List("n must be <= to 100")))
      .combine(Validated.cond(!(2 until n-1).exists(n % _ == 0), n, List("n must be prime")))
  }

  // Validated instances can be chained
  // if the original value is invalid, the chain stops and the errors do not accumulate
  validValue.andThen(_ => invalidValue)
  // test a valid value (turns a valid value into an invalid value based on a predicate)
  validValue.ensure(List("Something went wrong."))(_ % 2 == 0)
  // transform
  validValue.map(_ + 1) // value type
  validValue.leftMap(_.length) // error type
  validValue.bimap(_.length, _ + 1) // both
  // interoperate with Scala data structures
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(42))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(None, List("Nothing present here."))
  val tryToValidated: Validated[Throwable, Int] = Validated.fromTry(Try("something".toInt))
  //
  validValue.toOption
  validValue.toEither

  object FormValidation {
    //implicit val combineLinkedList: Semigroup[List[String]] = Semigroup.instance[List[String]](_ ::: _)
    implicit val combineStringForm: Semigroup[String] = Semigroup.instance[String]((s1, _) => s1)

    type FormValidation[T] = Validated[List[String], T]

    def validateForm(form: Map[String, String]): FormValidation[String] = {
      /*
        Fields: name, email, password
        Rules: All must be specified (any blank => error), email must have @, password must have >= 10 characters

        If the form was successful, return a Valid with "Success". Otherwise, display all errors.
      */
      // each returns a Valid or Invalid instance (Validated subtype)
      val name: Validated[List[String], String] = Validated
        .fromOption(form.get("name"), List("Name must be specified."))
        .ensure(List("Name must not be blank."))(_.nonEmpty)

      val email: Validated[List[String], String] = Validated
        .fromOption(form.get("email"), List("Email must be specified."))
        .ensure(List("Email must be valid."))(_.contains('@'))

      val password: Validated[List[String], String] = Validated
        .fromOption(form.get("password"), List("Password must be specified."))
        .ensure(List("Password must have at least 10 characters."))(_.length >= 10)

      val result: Validated[List[String], String] = name.combine(email).combine(password)

      if (result.isValid) Valid("Success!") else result
    }
  }


  def main(args: Array[String]): Unit = {
    //
    println(testNumber(6))
    println(validateNumber(-7))
    println(validateNumber(2))

    val result = FormValidation.validateForm(
      Map(
        "name" -> "Charles",
        "email" -> "info@carloslaraai.com",
        "password" -> "avalidpassword"
      )
    )
    println(result)
  }
}
