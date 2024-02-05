package part3datamanipulation

import cats.Semigroup

import scala.annotation.tailrec
import scala.util.Try

object DataValidation {

  import cats.data.Validated

  val aValidValue: Validated[String, Int] = Validated.valid(42) //right value
  val anUnValidValue: Validated[String, Int] = Validated.invalid("Something went wrong") // "left" value

  val aTest: Validated[String, Int] = Validated.cond(42 > 39, 99, "meaning of life is too small")

  def testPrime(n: Int): Boolean= {
    @tailrec
    def tailRecPrime(d: Int): Boolean =
      if (d < 1) true
      else n % d != 0 && tailRecPrime(d - 1)


    if (n == 0 || n ==1 || n == -1) false
    else tailRecPrime(Math.abs(n / 2))
  }


  // TODO: use Either
  // test whether the number
  // -n must be a prime
  // - n must be non - negative
  // n <= 100
  // n must be even
  //
  def testNumber(n: Int): Either[List[String], Int] = {
    val isNotEven: List[String] = if (n % 2 == 0) Nil else List("Number must be even")
    val isNegative: List[String] = if (n >= 0) Nil else List("Number must be non-negative")
    val isToBig: List[String] = if (n <= 100) Nil else List("Number must be less than or equal to 100")
    val isNotPrime: List[String] = if (testPrime(n)) Nil else List("Number must be a prime")
    if (n % 2 == 0 && n >= 10 && testPrime(n)) Right(n)
    else Left(isNotEven ++ isNegative ++ isToBig ++ isNotPrime)
  }

//  def testNumber2(n: Int): Either[List[String], Int] = {
//    val lst: List[Int => Either[List[String], Int]] = List[Int => Boolean]({
//      if (n >= 0) Nil else List("Number must be non-negative")
//    }, {
//      if (n % 2 == 0) Nil else List("Number must be even")
//    }, {
//      if (n <= 100) Nil else List("Number must be less than or equal to 100")
//    }, {
//      if (testPrime(n)) Nil else List("Number must be a prime")
//    })
//
//    for (elem <- lst) {
//      e
//    }
//  }
  import cats.instances.list._
  implicit val combineIntMax: Semigroup[Int]  = Semigroup.instance(Math.max)
  def validateNumber(n: Int): Validated[List[String], Int] = {
    Validated.cond(n % 2 == 0, n, List("Number must ne even"))
      .combine(Validated.cond(n > 0, n, List("Number must be non-negative")))
      .combine(Validated.cond(n <= 100, n, List("Number must be <= 100")))
      .combine(Validated.cond(testPrime(n), n, List("Number must be a prime")))
  }

  // chain
  aValidValue.andThen(_ => anUnValidValue)
  // test a valid value
  aValidValue.ensure(List("Something went wrong"))(_ % 2 == 0)
  // transform
  aValidValue.map(_ + 1)
  aValidValue.leftMap(_.length)
  aValidValue.bimap(_.length, _ + 1)
  // interoperate with stdlib
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(42))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(None, List("Nothing present here"))
  val tryToValidated: Validated[Throwable, Int] = Validated.fromTry(Try("something".toInt))
  //backwards
  aValidValue.toOption
  aValidValue.toEither

  //TODO 2 - form validation
  object FormValidation {

    type FormValidation[T] = Validated[List[String], T]

    def getValue(form: Map[String, String], fieldName: String): FormValidation[String] =
      Validated.fromOption(form.get(fieldName), List(s"The field $fieldName must be specified"))

    def nonBlank(value: String, fieldName: String): FormValidation[String] =
      Validated.cond(value.nonEmpty , value, List(s"the field $fieldName must not be blank"))

    /**
     * fields are
     * name
     * email
     * password
     *
     * rules are
     *  name, email and password MUST be specified
     *  name must not be blank
     *  email must have "@"
     *  password must have >=10 errors
     * @return
     */
      import cats.instances.list._
    def getSemigroup[T](f: (T, T) => T) : Semigroup[T] = {
      Semigroup.instance[T](f)
    }
    implicit val stringSemi: Semigroup[String] = getSemigroup[String]((a, _) => a)

    def validateForm(form: Map[String, String]) : FormValidation[String] = {
//      Validated.cond(form.contains("name"), "ok", List("name must be specified"))

      //TODO make with and then
        getValue(form, "name").andThen(name => nonBlank(name, "name"))
        .combine(getValue(form, "email"))
        .combine(getValue(form, "password"))
        .combine(Validated.cond(form.getOrElse("name", "").nonEmpty, "ok", List("name must not be blank")))
        .combine(Validated.cond(form.getOrElse("email", "").contains("@"), "ok", List("name must not be blank")))
        .combine(Validated.cond(form.getOrElse("password", "").length >= 10, "ok", List("email must have \"@\"")))
          .map(_ => "User registration complete.")
    }


  }


  //wraps
  import cats.syntax.validated._
  val aValidMeaningOfLife: Validated[List[String], Int] = 42.valid[List[String]]
  val anError: Validated[String, Int] = "Something went wrong".invalid[Int]



  def main(args: Array[String]): Unit = {
    println(FormValidation.validateForm(Map(
      "email" -> "bassboosted@gmail.com",
      "password" -> "dsadsdasadasd",
      "name" -> "ian"
    )))
  }

}
