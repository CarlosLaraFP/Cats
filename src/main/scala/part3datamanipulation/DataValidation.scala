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
  // if the original value is invalid, the chain continues and the errors accumulate
  validValue.andThen(_ => invalidValue)
  // test a valid value (turns a valid value into an invalid value based on a predicate)
  validValue.ensure(List("Something went wrong."))(_ % 2 == 0)
  // transform
  validValue.map(_ + 1) // value type
  validValue.leftMap(_.length) // error type
  validValue.bimap(_.length, _ + 1) // both
  // interoperate with Scala data structures (standard library)
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(42))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(None, List("Nothing present here."))
  val tryToValidated: Validated[Throwable, Int] = Validated.fromTry(Try("something".toInt))
  // backwards
  validValue.toOption
  validValue.toEither

  object FormValidation {
    //implicit val combineLinkedList: Semigroup[List[String]] = Semigroup.instance[List[String]](_ ::: _)
    //implicit val combineStringForm: Semigroup[String] = Semigroup.instance[String]((_, _) => "Success!")
    import cats.instances.string._ // default concatenation is fine

    type FormValidation[T] = Validated[List[String], T]

    def getValue(form: Map[String, String], fieldName: String): FormValidation[String] =
      Validated.fromOption(form.get(fieldName), List(s"$fieldName must be specified."))

    def nonBlank(value: String, fieldName: String): FormValidation[String] =
      Validated.cond(value.nonEmpty, value, List(s"$fieldName must not be blank."))

    def validEmail(email: String): FormValidation[String] =
      Validated.cond(email.contains('@'), email, List("Email must be valid."))

    def validPassword(password: String): FormValidation[String] =
      Validated.cond(password.length >= 10, password, List("Password must have at least 10 characters."))

    def validateForm(form: Map[String, String]): FormValidation[String] =
      /*
        Fields: name, email, password
        Rules: All must be specified (any blank => error), email must have @, password must have >= 10 characters

        If the form was successful, return a Valid with "Success". Otherwise, display all errors.
      */
      getValue(form, "Name")
        .andThen(name => nonBlank(name, "Name"))
        .combine(getValue(form, "Email")
          .andThen(validEmail))
        .combine(getValue(form, "Password")
          .andThen(validPassword))
        .map(_ => "User registration successful!")
  }

  // Validated has useful extension methods for a nicer API
  import cats.syntax.validated._

  val validMeaningOfLife: Validated[List[String], Int] = 42.valid
  val anError: Validated[String, Int] = "Something went wrong".invalid


  def main(args: Array[String]): Unit = {
    //
    println(testNumber(6))
    println(validateNumber(-7))
    println(validateNumber(2))

    val result = FormValidation.validateForm(
      Map(
        "Name" -> "Charles",
        "Email" -> "info@carloslaraai.com",
        "Password" -> "ValidPassword"
      )
    )
    println(result)
  }
}
